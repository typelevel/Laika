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
import laika.collection.TransitionalCollectionOps._
import laika.config._
import laika.parse.Failure

import scala.collection.mutable

/** Translates the interim configuration model (usually obtained from a HOCON parser)
  * into the final object model. It turns a root `ObjectBuilderValue` into
  * a root `ObjectValue`.
  * 
  * The translation step involves the following steps:
  * 
  * - Expand keys (e.g. `{ a.b.c = 7 }` will become `{ a = { b = { c = 7 }}}`
  * - Merge objects with a common base path
  * - Merge concatenated values (e.g. `[1,2] [3,4]` will become `[1,2,3,4]`
  * - Resolve substitution variables (potentially using the provided fallback if not found in
  *   in the provided unresolved root)
  * 
  * @author Jens Halm
  */
object ConfigResolver {

  /** Translates the interim configuration model (usually obtained from a HOCON parser)
    * into the final object model. It turns a root `ObjectBuilderValue` into
    * a root `ObjectValue`.
    *
    * The translation step involves the following steps:
    *
    * - Expand keys (e.g. `{ a.b.c = 7 }` will become `{ a = { b = { c = 7 }}}`
    * - Merge objects with a common base path
    * - Merge concatenated values (e.g. `[1,2] [3,4]` will become `[1,2,3,4]`
    * - Resolve substitution variables (potentially using the provided fallback if not found in
    *   in the provided unresolved root)
    */
  def resolve(root: ObjectBuilderValue, origin: Origin, fallback: Config): Either[ConfigError, ObjectValue] = {

    val errors = extractErrors(root)

    if (errors.nonEmpty) Left(ConfigParserErrors(errors))
    else {

      val rootExpanded = mergeObjects(expandPaths(root))

      def renderPath(p: Path): String = p.components.mkString(".")

      val activeFields = mutable.Set.empty[Path]
      val resolvedFields = mutable.Map.empty[Path, ConfigValue]
      val startedObjects = mutable.Map.empty[Path, ObjectBuilderValue] // may be in progress or resolved
      val invalidPaths = mutable.Map.empty[Path, String]

      def resolvedValue(path: Path): Option[ConfigValue] = resolvedFields.get(path)

      def deepMerge(o1: ObjectValue, o2: ObjectValue): ObjectValue = {
        val resolvedFields = (o1.values ++ o2.values).groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
          case (name, values) => Field(name, values.reduce(merge), origin)
        }
        ObjectValue(resolvedFields.sortBy(_.key))
      }

      def resolveValue(path: Path)(value: ConfigBuilderValue): Option[ConfigValue] = value match {
        case o: ObjectBuilderValue => Some(resolveObject(o, path))
        case a: ArrayBuilderValue => Some(ArrayValue(a.values.flatMap(resolveValue(path)))) // TODO - adjust path?
        case r: ResolvedBuilderValue => Some(r.value)
        case s: ValidStringValue => Some(StringValue(s.value))
        case c: ConcatValue => c.allParts.flatMap(resolveConcatPart(path)).reduceOption(concat(path))
        case m: MergedValue => resolveMergedValue(path: Path)(m.values.reverse)
        case SelfReference => None
        case _: InvalidStringValue => None
        case SubstitutionValue(ref, true) if ref == path => None
        case SubstitutionValue(ref, optional) =>
          resolvedValue(ref).orElse(lookahead(ref)).orElse {
            if (!optional) invalidPaths += ((path, s"Missing required reference: '${renderPath(ref)}'"))
            None
          }
      }

      def resolveMergedValue(path: Path)(values: Seq[ConfigBuilderValue]): Option[ConfigValue] = {

        def loop(values: Seq[ConfigBuilderValue]): Option[ConfigValue] = (resolveValue(path)(values.head), values.tail) match {
          case (Some(ov: ObjectValue), Nil) => Some(ov)
          case (Some(ov: ObjectValue), rest) => loop(rest) match {
            case Some(o2: ObjectValue) => Some(merge(o2, ov))
            case _ => Some(ov)
          }
          case (Some(other), _) => Some(other)
          case (None, Nil) => None
          case (None, rest) => loop(rest)
        }

        loop(values)
      }

      def lookahead(path: Path): Option[ConfigValue] = {

        def resolvedParent(current: Path): Option[(ObjectBuilderValue, Path)] = {
          if (current == Path.Root) Some((rootExpanded, current))
          else {
            val matching = startedObjects.toSeq.filter(o => current.isSubPath(o._1))
            val sorted = matching.sortBy(_._1.components.length)
            sorted.lastOption.fold(resolvedParent(current.parent)) {
              case (commonPath, obv) => Some((obv, Path(Path.Root, current.components.take(commonPath.components.size + 1))))
            }
          }
        }

        if (activeFields.contains(path)) {
          invalidPaths += ((path, s"Circular Reference involving path '${renderPath(path)}'"))
          Some(NullValue)
        } else {
          resolvedParent(path).flatMap { case (obj, fieldPath) =>
            obj.values.find(_.validKey == fieldPath).map(_.value).foreach(resolveField(fieldPath, _, obj))
            resolvedValue(path).orElse(fallback.get[ConfigValue](path).toOption)
          }
        }
      }

      def resolveConcatPart(path: Path)(part: ConcatPart): Option[ConfigValue] = part.value match {
        case SelfReference => None
        case other => resolveValue(path)(other) match {
          case Some(simpleValue: SimpleConfigValue) => Some(StringValue(part.whitespace + simpleValue.render))
          case Some(_: ASTValue) => None
          case other => other
        }
      }

      def concat(path: Path)(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
        (v1, v2) match {
          case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
          case (a1: ArrayValue, a2: ArrayValue) => ArrayValue(a1.values ++ a2.values)
          case (s1: StringValue, s2: StringValue) => StringValue(s1.value ++ s2.value)
          case (NullValue, a2: ArrayValue) => a2
          case _ =>
            invalidPaths += ((path, s"Invalid concatenation of values. It must contain either only objects, only arrays or only simple values"))
            NullValue
        }
      }

      def merge(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
        (v1, v2) match {
          case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
          case (_, c2) => c2
        }
      }

      def resolveField(path: Path, value: ConfigBuilderValue, parent: ObjectBuilderValue): Option[ConfigValue] = {
        resolvedValue(path).orElse {
          activeFields += path
          val res = resolveValue(path)(value)
          activeFields -= path
          res.foreach { resolved =>
            resolvedFields += ((path, resolved))
          }
          res
        }
      }

      def resolveObject(obj: ObjectBuilderValue, path: Path): ObjectValue = {
        startedObjects += ((path, obj))
        val resolvedFields = obj.values.flatMap { field =>
          resolveField(field.validKey, field.value, obj).map(Field(field.validKey.name, _, origin))
        }
        ObjectValue(resolvedFields.sortBy(_.key))
      }

      val res = resolveObject(rootExpanded, Path.Root)

      if (invalidPaths.nonEmpty) Left(ConfigResolverError(
        s"One or more errors resolving configuration: ${invalidPaths.map { case (key, msg) => s"'${renderPath(key)}': $msg" }.mkString(", ")}"
      ))
      else Right(res)
    }
  }

  /** Merges objects with a common base path into a single one.
    * 
    * ```
    * a = { b = { c = 7 }}
    * 
    * a = { b = { d = 9 }}
    * ```
    * 
    * will become
    *
    * ```
    * a = { b = { c = 7, d = 9 }}
    * ```
    *
    * @param obj
    * @return
    */
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
    
    val mergedFields = obj.values.groupBy(_.key.right.get).mapValuesStrict(_.map(_.value)).toSeq.map {
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
      field.validKey.components match {
        case name :: Nil => 
          field.copy(
            key = Right(path / name), 
            value = expandValue(field.value, path / name)
          )
        case name :: rest =>
          field.copy(
            key = Right(path / name), 
            value = expandPaths(ObjectBuilderValue(Seq(BuilderField(Right(Path(rest)), field.value))), path / name)
          )
        case Nil =>
          field.copy(
            key = Right(Path.Root),
            value = expandValue(field.value, Path.Root) // TODO - 0.13 - should never get here
          )
      }
    }
    obj.copy(values = expandedFields)
  }

  /* Extracts all invalid values from the unresolved config tree */
  def extractErrors (obj: ObjectBuilderValue): Seq[Failure] = {

    def extract(value: ConfigBuilderValue): Seq[Failure] = value match {
      case InvalidStringValue(_, failure)  => Seq(failure)
      case InvalidBuilderValue(ArrayBuilderValue(values), failure) => 
        val nested = values.flatMap(extract) 
        if (nested.isEmpty) Seq(failure) else nested
      case InvalidBuilderValue(obj: ObjectBuilderValue, failure) => 
        val nested = extractErrors(obj)
        if (nested.isEmpty) Seq(failure) else nested
      case InvalidBuilderValue(_, failure) => Seq(failure)
      case child: ObjectBuilderValue       => extractErrors(child)
      case child: ArrayBuilderValue        => child.values.flatMap(extract)
      case concat: ConcatValue             => (concat.first +: concat.rest.map(_.value)).flatMap(extract)
      case _                               => Nil
    }

    obj.values.flatMap {
      case BuilderField(Left(InvalidStringValue(_, failure)), inv: InvalidBuilderValue) if failure.message.contains("unquoted string") => 
        Seq(inv.failure.copy(maxOffset = failure.maxOffset))
      case BuilderField(Left(InvalidStringValue(_, failure)), _) => Seq(failure)
      case BuilderField(_, value) => extract(value)
    }
  }
  
}
