/*
 * Copyright 2012-2020 the original author or authors.
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

import laika.api.config.{ Config, ConfigError, ConfigValue, Field, Key, Origin }
import laika.internal.collection.TransitionalCollectionOps.*
import laika.api.config.Config.IncludeMap
import laika.api.config.ConfigError.{ ConfigParserErrors, ConfigResolverError }
import laika.api.config.ConfigValue.*
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
private[laika] object ConfigResolver {

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
  def resolve(
      root: ObjectBuilderValue,
      origin: Origin,
      fallback: Config,
      includes: IncludeMap
  ): Either[ConfigError, ObjectValue] = {

    val errors = extractErrors(root)

    if (errors.nonEmpty) Left(ConfigParserErrors(errors))
    else {

      val rootExpanded = mergeObjects(expandPaths(root))

      val activeFields   = mutable.Set.empty[Key]
      val resolvedFields = mutable.Map.empty[Key, ConfigValue]
      val startedObjects =
        mutable.Map.empty[Key, ObjectBuilderValue] // may be in progress or resolved
      val invalidPaths = mutable.Map.empty[Key, String]

      def resolvedValue(path: Key): Option[ConfigValue] = resolvedFields.get(path)

      def deepMerge(o1: ObjectValue, o2: ObjectValue): ObjectValue = {
        val resolvedFields =
          (o1.values ++ o2.values).groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
            case (name, values) => Field(name, values.reduce(merge), origin)
          }
        ObjectValue(resolvedFields.sortBy(_.key))
      }

      def resolveValue(key: Key)(value: ConfigBuilderValue): Option[ConfigValue] = value match {
        case o: ObjectBuilderValue   => Some(resolveObject(o, key))
        case a: ArrayBuilderValue    =>
          Some(ArrayValue(a.values.flatMap(resolveValue(key)))) // TODO - adjust path?
        case r: ResolvedBuilderValue => Some(r.value)
        case s: ValidStringValue     => Some(StringValue(s.value))
        case c: ConcatValue => c.allParts.flatMap(resolveConcatPart(key)).reduceOption(concat(key))
        case m: MergedValue => resolveMergedValue(key: Key)(m.values.reverse)
        case SelfReference  => None
        case _: InvalidStringValue                      => None
        case SubstitutionValue(ref, true) if ref == key => None
        case SubstitutionValue(ref, optional)           =>
          resolvedValue(ref).orElse(lookahead(ref)).orElse {
            if (!optional) invalidPaths += ((key, s"Missing required reference: '$ref'"))
            None
          }
        case _                                          => None
      }

      def resolveMergedValue(key: Key)(values: Seq[ConfigBuilderValue]): Option[ConfigValue] = {

        def loop(values: Seq[ConfigBuilderValue]): Option[ConfigValue] =
          (resolveValue(key)(values.head), values.tail) match {
            case (Some(ov: ObjectValue), Nil)  => Some(ov)
            case (Some(ov: ObjectValue), rest) =>
              loop(rest) match {
                case Some(o2: ObjectValue) => Some(merge(o2, ov))
                case _                     => Some(ov)
              }
            case (Some(other), _)              => Some(other)
            case (None, Nil)                   => None
            case (None, rest)                  => loop(rest)
          }

        loop(values)
      }

      def lookahead(key: Key): Option[ConfigValue] = {

        def resolvedParent(current: Key): Option[(ObjectBuilderValue, Key)] = {
          if (current.segments.isEmpty) Some((rootExpanded, current))
          else {
            val matching = startedObjects.toSeq.filter(o => current.isChild(o._1))
            val sorted   = matching.sortBy(_._1.segments.length)
            sorted.lastOption.fold(resolvedParent(current.parent)) { case (commonPath, obv) =>
              Some((obv, Key(current.segments.take(commonPath.segments.size + 1))))
            }
          }
        }

        if (activeFields.contains(key)) {
          invalidPaths += ((key, s"Circular Reference involving path '$key'"))
          Some(NullValue)
        }
        else {
          resolvedParent(key).flatMap { case (obj, fieldPath) =>
            obj.values.find(_.validKey == fieldPath).map(_.value).foreach(
              resolveField(fieldPath, _)
            )
            resolvedValue(key).orElse(fallback.get[ConfigValue](key).toOption)
          }
        }
      }

      def resolveConcatPart(key: Key)(part: ConcatPart): Option[ConfigValue] = part.value match {
        case SelfReference => None
        case other         =>
          resolveValue(key)(other) match {
            case Some(simpleValue: SimpleValue) =>
              Some(StringValue(part.whitespace + simpleValue.render))
            case Some(_: ASTValue)              => None
            case other                          => other
          }
      }

      def concat(key: Key)(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
        (v1, v2) match {
          case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
          case (a1: ArrayValue, a2: ArrayValue)   => ArrayValue(a1.values ++ a2.values)
          case (s1: StringValue, s2: StringValue) => StringValue(s1.value ++ s2.value)
          case (NullValue, a2: ArrayValue)        => a2
          case _                                  =>
            invalidPaths += ((
              key,
              s"Invalid concatenation of values. It must contain either only objects, only arrays or only simple values"
            ))
            NullValue
        }
      }

      def merge(v1: ConfigValue, v2: ConfigValue): ConfigValue = {
        (v1, v2) match {
          case (o1: ObjectValue, o2: ObjectValue) => deepMerge(o1, o2)
          case (_, c2)                            => c2
        }
      }

      def resolveField(
          key: Key,
          value: ConfigBuilderValue
      ): Option[ConfigValue] = {
        resolvedValue(key).orElse {
          activeFields += key
          val res = resolveValue(key)(value)
          activeFields -= key
          res.foreach { resolved =>
            resolvedFields += ((key, resolved))
          }
          res
        }
      }

      def resolveObject(obj: ObjectBuilderValue, key: Key): ObjectValue = {
        startedObjects += ((key, obj))

        def resolve(field: BuilderField): Option[Field] =
          resolveField(field.validKey, field.value).map(
            Field(field.validKey.local.toString, _, origin)
          )

        val resolvedFields = obj.values.flatMap {
          case BuilderField(_, IncludeBuilderValue(resource)) =>
            includes.get(resource) match {
              case None                  =>
                if (resource.isRequired)
                  invalidPaths += ((
                    key,
                    s"Missing required include '${resource.resourceId.value}'"
                  ))
                Nil
              case Some(Left(error))     =>
                invalidPaths += ((
                  key,
                  s"Error including '${resource.resourceId.value}': ${error.message}"
                ))
                Nil
              case Some(Right(included)) =>
                resolveObject(included, key).values
            }
          case field                                          => resolve(field)
        }
        ObjectValue(resolvedFields.sortBy(_.key))
      }

      val res = resolveObject(rootExpanded, Key.root)

      if (invalidPaths.nonEmpty)
        Left(
          ConfigResolverError(
            s"One or more errors resolving configuration: ${
                invalidPaths.map { case (key, msg) => s"'$key': $msg" }.mkString(", ")
              }"
          )
        )
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

    def resolveSelfReference(
        key: Key,
        value: ConcatValue,
        parent: ConfigBuilderValue
    ): ConfigBuilderValue = {
      def resolve(value: ConfigBuilderValue): ConfigBuilderValue = value match {
        case SelfReference                           => parent
        case SubstitutionValue(ref, _) if ref == key => parent
        case other                                   => other
      }
      val resolved                                               =
        ConcatValue(resolve(value.first), value.rest.map(p => p.copy(value = resolve(p.value))))
      if (resolved == value) MergedValue(Seq(parent, value)) else resolved
    }

    def mergeValues(
        key: Key
    )(cbv1: ConfigBuilderValue, cbv2: ConfigBuilderValue): ConfigBuilderValue = (cbv1, cbv2) match {
      case (o1: ObjectBuilderValue, o2: ObjectBuilderValue) =>
        mergeObjects(ObjectBuilderValue(o1.values ++ o2.values))
      case (v1, SelfReference)                              => v1
      case (v1, SubstitutionValue(ref, _)) if ref == key    => v1
      case (_, r2: ResolvedBuilderValue)                    => r2
      case (_, a2: ArrayBuilderValue)                       => a2
      case (_, o2: ObjectBuilderValue)                      => o2
      case (v1, c2: ConcatValue)                            => resolveSelfReference(key, c2, v1)
      case (MergedValue(vs), v2)                            => MergedValue(vs :+ v2)
      case (v1, v2)                                         => MergedValue(Seq(v1, v2))
    }

    val mergedFields =
      obj.values.groupBy(_.key.getOrElse(Key.root)).mapValuesStrict(_.map(_.value)).toSeq.map {
        case (key, values) =>
          val merged = values.reduce(mergeValues(key)) match {
            case obj: ObjectBuilderValue => mergeObjects(obj)
            case other                   => other
          }
          BuilderField(key, merged)
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
  def expandPaths(obj: ObjectBuilderValue, key: Key = Key.root): ObjectBuilderValue = {

    def expandValue(value: ConfigBuilderValue, child: Key): ConfigBuilderValue = value match {
      case o: ObjectBuilderValue => expandPaths(o, child)
      case a: ArrayBuilderValue  =>
        val expandedElements = a.values.zipWithIndex.map { case (element, index) =>
          expandValue(element, child.child(index.toString))
        }
        ArrayBuilderValue(expandedElements)
      case c: ConcatValue        =>
        c.copy(
          first = expandValue(c.first, child),
          rest = c.rest.map(part => part.copy(value = expandValue(part.value, child)))
        )
      case other                 => other
    }

    val expandedFields = obj.values.map { field =>
      field.validKey.segments.toList match {
        case name :: Nil  =>
          field.copy(
            key = Right(key.child(name)),
            value = expandValue(field.value, key.child(name))
          )
        case name :: rest =>
          field.copy(
            key = Right(key.child(name)),
            value = expandPaths(
              ObjectBuilderValue(Seq(BuilderField(Right(Key(rest)), field.value))),
              key.child(name)
            )
          )
        case Nil          =>
          field.copy(
            key = Right(Key.root),
            value = expandValue(field.value, Key.root) // TODO - should never get here
          )
      }
    }
    obj.copy(values = expandedFields)
  }

  /* Extracts all invalid values from the unresolved config tree */
  def extractErrors(obj: ObjectBuilderValue): Seq[Failure] = {

    def extract(value: ConfigBuilderValue): Seq[Failure] = value match {
      case InvalidStringValue(_, failure)                          => Seq(failure)
      case InvalidBuilderValue(ArrayBuilderValue(values), failure) =>
        val nested = values.flatMap(extract)
        if (nested.isEmpty) Seq(failure) else nested
      case InvalidBuilderValue(obj: ObjectBuilderValue, failure)   =>
        val nested = extractErrors(obj)
        if (nested.isEmpty) Seq(failure) else nested
      case InvalidBuilderValue(_, failure)                         => Seq(failure)
      case incl: IncludeBuilderValue                               =>
        incl.resource.resourceId match {
          case InvalidStringValue(_, failure) => Seq(failure)
          case _                              => Nil
        }
      case child: ObjectBuilderValue                               => extractErrors(child)
      case child: ArrayBuilderValue                                => child.values.flatMap(extract)
      case concat: ConcatValue => (concat.first +: concat.rest.map(_.value)).flatMap(extract)
      case _                   => Nil
    }

    obj.values.flatMap {
      case BuilderField(Left(InvalidStringValue(_, failure)), inv: InvalidBuilderValue)
          if failure.message.contains("unquoted string") =>
        // keep the message of the value failure with the position of the key failure for more clarity
        Seq(failure.copy(msgProvider = inv.failure.msgProvider))
      case BuilderField(Left(InvalidStringValue(_, failure)), _) => Seq(failure)
      case BuilderField(_, value)                                => extract(value)
    }
  }

}
