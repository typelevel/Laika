/*
 * Copyright 2012-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.parse.hocon

import laika.ast.Path
import laika.parse.hocon.HoconParsers._
import laika.collection.TransitionalCollectionOps._

import scala.collection.mutable

/**
  * @author Jens Halm
  */
object ConfigResolver {

  def resolve(root: ObjectBuilderValue): ObjectValue = {
    
    val rootExpanded = mergeObjects(expandPaths(root))
    
    println(s"resolving root: $rootExpanded")
    
    val activeFields = mutable.Set.empty[Path]
    val resolvedFields = mutable.Map.empty[Path, ConfigValue]
    val startedObjects = mutable.Map.empty[Path, ObjectBuilderValue] // may be in progress or resolved
    val invalidPaths = mutable.Map.empty[Path, String]
    
    def resolvedValue(path: Path): Option[ConfigValue] = resolvedFields.get(path)
    
    def deepMerge(o1: ObjectValue, o2: ObjectValue): ObjectValue =  {
      val resolvedFields = (o1.values ++ o2.values).groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (name, values) => Field(name, values.reduce(merge))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }

    def resolveValue(path: Path)(value: ConfigBuilderValue): ConfigValue = value match {
      case o: ObjectBuilderValue => resolveObject(o, path)
      case a: ArrayBuilderValue => ArrayValue(a.values.map(resolveValue(path))) // TODO - adjust path
      case r: ResolvedBuilderValue => r.value
      case c: ConcatValue => c.allParts.map(resolveConcatPart(path)).reduce(concat)
      case m: MergedValue => resolveMergedValue(path: Path)(m.values.reverse)
      case SelfReference => NullValue
      case SubstitutionValue(ref, optional) =>
        println(s"resolve ref '${ref.toString}'")
        resolvedValue(ref).orElse(lookahead(ref)).getOrElse(NullValue) // TODO - error if not optional
    }
    
    def resolveMergedValue(path: Path)(values: Seq[ConfigBuilderValue]): ConfigValue = {
      
      def loop(values: Seq[ConfigBuilderValue]): ConfigValue = (resolveValue(path)(values.head), values.tail) match {
        case (ov: ObjectValue, Nil) => ov
        case (ov: ObjectValue, rest) => loop(rest) match {
          case o2: ObjectValue => merge(o2, ov)
          case _ => ov
        }
        case (other, _) => other
      }
      
      loop(values)
    }
    
    def lookahead(path: Path): Option[ConfigValue] = {
      
      def resolvedParent(current: Path): Option[(ObjectBuilderValue, Path)] = {
        if (current == Path.Root) Some((rootExpanded, current))
        else {
          val matching = startedObjects.toSeq.filter(o => current.isSubPath(o._1))
          val sorted = matching.sortBy(_._1.components.length)
          println(s"matching active objects for path $current: ${sorted.map(_._1.toString).mkString(" ")}")
          sorted.lastOption.fold(resolvedParent(current.parent)) {
            case (commonPath, obv) => Some((obv, Path(Path.Root, current.components.take(commonPath.components.size + 1))))
          }
        }
      } 
      
      resolvedParent(path).flatMap { case (obj, fieldPath) =>
        println(s"lookahead from '${fieldPath.toString}'")
        println(s"keys before lookahead: ${resolvedFields.keySet.map(_.toString).mkString(" ")}")
        println(s"keys in selected parent: ${obj.values.map(_.key.toString).mkString(" ")}")
        resolveField(fieldPath, obj.values.find(_.key == fieldPath).map(_.value).get, obj) // TODO - handle None
        println(s"keys after lookahead: ${resolvedFields.keySet.map(_.toString).mkString(" ")}")
        val res = resolvedValue(path)
        println(s"success? ${res.isDefined}")
        res
      }
    }
    
    def resolveConcatPart(path: Path)(part: ConcatPart): ConfigValue = part.value match {
      case SelfReference => NullValue
      case other => resolveValue(path)(other) match {
        case NullValue       => StringValue(part.whitespace + "null")
        case BooleanValue(v) => StringValue(part.whitespace + v.toString)
        case LongValue(v)    => StringValue(part.whitespace + v.toString)
        case DoubleValue(v)  => StringValue(part.whitespace + v.toString)
        case StringValue(v)  => StringValue(part.whitespace + v.toString)
        case arrayOrObject   => arrayOrObject
      }
    } 

    def concat(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
      (v1, v2) match {
        case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
        case (a1: ArrayValue, a2: ArrayValue) => ArrayValue(a1.values ++ a2.values)
        case (s1: StringValue, s2: StringValue) => StringValue(s1.value ++ s2.value)
        case (NullValue, a2: ArrayValue) => a2
        case (c1, c2) => NullValue // TODO - invalid combination of concat values
      }
    }
    
    def merge(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
      (v1, v2) match {
        case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
        case (_, c2) => c2
      }
    }
    
    def resolveField(path: Path, value: ConfigBuilderValue, parent: ObjectBuilderValue): ConfigValue = {
      resolvedValue(path).getOrElse {
        println(s"resolve field '${path.toString}'")
        activeFields += path
        val res = resolveValue(path)(value)
        activeFields -= path
        resolvedFields += ((path, res))
        res
      }
    }
    
    def resolveObject(obj: ObjectBuilderValue, path: Path): ObjectValue = {
      startedObjects += ((path, obj))
      println(s"resolve obj with keys: ${obj.values.map(_.key.toString).mkString(" ")}")
      val resolvedFields = obj.values.map { field =>
        Field(field.key.name, resolveField(field.key, field.value, obj))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }
    
    resolveObject(rootExpanded, Path.Root)
  }
  
  def mergeObjects(obj: ObjectBuilderValue): ObjectBuilderValue = {

    def resolveSelfReference(path: Path, value: ConcatValue, parent: ConfigBuilderValue): ConfigBuilderValue = {
      def resolve (value: ConfigBuilderValue): ConfigBuilderValue = value match {
        case SelfReference => parent
        case SubstitutionValue(ref, _) if ref == path => parent
        case other => other
      }
      val resolved = ConcatValue(resolve(value.first), value.rest.map(p => p.copy(value = resolve(p.value))))
      if (resolved == value) MergedValue(Seq(parent, value)) else resolved
    }
    
    def mergeValues(path: Path)(cbv1: ConfigBuilderValue, cbv2: ConfigBuilderValue): ConfigBuilderValue = (cbv1, cbv2) match {
      case (o1: ObjectBuilderValue, o2: ObjectBuilderValue) => mergeObjects(ObjectBuilderValue(o1.values ++ o2.values))
      case (v1, SelfReference) => v1
      case (v1, SubstitutionValue(ref, _)) if ref == path => v1
      case (_, r2: ResolvedBuilderValue) => r2
      case (_, a2: ArrayBuilderValue) => a2
      case (_, o2: ObjectBuilderValue) => o2
      case (v1, c2: ConcatValue) => resolveSelfReference(path, c2, v1)
      case (MergedValue(vs), v2) => MergedValue(vs :+ v2) 
      case (v1, v2) => MergedValue(Seq(v1, v2))
    }
    
    val mergedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
      case (path, values) => BuilderField(path.name, values.reduce(mergeValues(path)))
    }
    ObjectBuilderValue(mergedFields)
  }
   
  /** Expands all flattened path expressions to nested objects.
    * 
    * ```
    * { a.b.c = 7 }
    * ```
    * 
    * will become
    * 
    * ```
    * { a = { b = { c = 7 }}}
    * ```
    */
  def expandPaths(obj: ObjectBuilderValue, path: Path = Path.Root): ObjectBuilderValue = {
    
    def expandValue(value: ConfigBuilderValue, child: Path): ConfigBuilderValue = value match {
      case o: ObjectBuilderValue => expandPaths(o, child)
      case a: ArrayBuilderValue => 
        val expandedElements = a.values.zipWithIndex.map { case (element, index) =>
          expandValue(element, child / index.toString)
        }
        ArrayBuilderValue(expandedElements)
      case c: ConcatValue => c.copy(
        first = expandValue(c.first, child),
        rest = c.rest.map(part => part.copy(value = expandValue(part.value, child)))
      )
      case other => other
    }
    
    val expandedFields = obj.values.map { field =>
      //println(s"expand key ${field.key.toString}")
      field.key.components match {
        case name :: Nil => 
          field.copy(
            key = path / name, 
            value = expandValue(field.value, path / name)
          )
        case name :: rest =>
          field.copy(
            key = path / name, 
            value = expandPaths(ObjectBuilderValue(Seq(BuilderField(Path(rest), field.value))), path / name)
          )
        case Nil =>
          field.copy(
            key = Path.Root,
            value = expandValue(field.value, Path.Root) // TODO - 0.12 - should never get here
          )
      }
    }
    //println(s"expanded keys: ${expandedFields.map(_.key.toString).mkString(" ")}")
    obj.copy(values = expandedFields)
  }
  
}
