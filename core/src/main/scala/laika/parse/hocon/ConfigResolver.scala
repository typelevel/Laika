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
import laika.parse.hocon.HoconParsers.{ArrayBuilderValue, ArrayValue, BooleanValue, BuilderField, ConcatPart, ConcatValue, ConfigBuilderValue, ConfigValue, DoubleValue, Field, LongValue, MergedValue, NullValue, ObjectBuilderValue, ObjectValue, ResolvedBuilderValue, SelfReference, StringValue, SubstitutionValue}
import laika.collection.TransitionalCollectionOps._

import scala.collection.mutable

/**
  * @author Jens Halm
  */
object ConfigResolver {

  def resolve(root: ObjectBuilderValue): ObjectValue = {
    
    val rootExpanded = mergeObjects(expandPaths(root))
    
    println(s"resolving root: $rootExpanded")
    
    val activePaths = mutable.Set.empty[Path]
    val resolvedPaths = mutable.Map.empty[Path, (ConfigValue, ConfigBuilderValue)]
    val invalidPaths = mutable.Map.empty[Path, String]
    
    def resolvedValue(path: Path): Option[ConfigValue] = resolvedPaths.get(path).map(_._1)
    
    def deepMerge(o1: ObjectValue, o2: ObjectValue): ObjectValue =  {
      val resolvedFields = (o1.values ++ o2.values).groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (name, values) => Field(name, values.reduce(merge))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }

    def resolveValue(value: ConfigBuilderValue): ConfigValue = value match {
      case o: ObjectBuilderValue => resolveObject(o)
      case a: ArrayBuilderValue => ArrayValue(a.values.map(resolveValue))
      case r: ResolvedBuilderValue => r.value
      case c: ConcatValue => c.allParts.map(resolveConcatPart).reduce(concat)
      case SelfReference => NullValue
      case SubstitutionValue(ref, optional) =>
        println(s"resolve ref '${ref.toString}'")
        resolvedValue(ref).orElse(lookahead(ref)).getOrElse(NullValue) // TODO - error if not optional
    }
    
    def lookahead(path: Path): Option[ConfigValue] = {
      
      def resolvedParent(current: Path): Option[(ObjectBuilderValue, Path)] = {
        val parent = current.parent
        if (parent == Path.Root) Some((rootExpanded, current))
        else resolvedPaths.get(parent).fold(resolvedParent(parent)) {
          case (_, obv: ObjectBuilderValue) => Some((obv, current))
          case _ => None
        }
      } 
      
      resolvedParent(path).flatMap { case (obj, fieldPath) =>
        println(s"lookahead from '${fieldPath.toString}'")
        println(s"keys before lookahead: ${resolvedPaths.keySet.map(_.toString).mkString(" ")}")
        resolveField(fieldPath, obj.values.filter(_.key == fieldPath).map(_.value), obj)
        println(s"keys after lookahead: ${resolvedPaths.keySet.map(_.toString).mkString(" ")}")
        val res = resolvedValue(path)
        println(s"success? ${res.isDefined}")
        res
      }
    }
    
    def resolveConcatPart(part: ConcatPart): ConfigValue = part.value match {
      case SelfReference => NullValue
      case other => resolveValue(other) match {
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
    
    def resolveField(path: Path, values: Seq[ConfigBuilderValue], parent: ObjectBuilderValue): ConfigValue = {
      resolvedValue(path).getOrElse {
        println(s"resolve field '${path.toString}'")
        activePaths += path
        val res = values.map(resolveValue).reduce(merge)
        activePaths -= path
        resolvedPaths += ((path, (res, parent)))
        res
      }
    }
    
    def resolveObject(obj: ObjectBuilderValue): ObjectValue = {
      println(s"resolve obj with keys: ${obj.values.map(_.key.toString).mkString(" ")}")
      val resolvedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (path, values) => Field(path.name, resolveField(path, values, obj))
      }
      ObjectValue(resolvedFields.sortBy(_.key))
    }
    
    resolveObject(rootExpanded)
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
      case c: ConcatValue => c.copy(
        first = expandValue(c.first, child),
        rest = c.rest.map(part => part.copy(value = expandValue(part.value, child)))
      )
      case other => other
    }
    
    val expandedFields = obj.values.map { field =>
      println(s"expand key ${field.key.toString}")
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
      }
    }
    println(s"expanded keys: ${expandedFields.map(_.key.toString).mkString(" ")}")
    obj.copy(values = expandedFields)
  }
  
}
