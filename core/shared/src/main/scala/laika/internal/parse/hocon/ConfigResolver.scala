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

package laika.internal.parse.hocon

import cats.data.NonEmptyChain
import laika.api.config.{ ConfigError, Key, Origin }
import laika.internal.collection.TransitionalCollectionOps.*
import laika.api.config.Config.IncludeMap
import laika.api.config.ConfigError.{ InvalidField, InvalidFields }
import laika.api.config.ConfigValue.*
import laika.parse.{ Failure, Message }

/** Translates the configuration builder model (usually obtained from a HOCON parser)
  * into and partially resolved interim model.
  * It turns a root `ObjectBuilderValue` into a root `FieldRef`.
  * The only fields the interim model leaves unresolved are those that need to
  * query a fallback `Config`, since the fallback can change
  * when a `Document` gets "re-homed" into a different `DocumentTree`.
  *
  * The translation step involves the following steps:
  *
  * - Expand keys (e.g. `{ a.b.c = 7 }` will become `{ a = { b = { c = 7 }}}`)
  * - Merge objects with a common base path
  * - Merge concatenated values (e.g. `[1,2] [3,4]` will become `[1,2,3,4]`
  * - Resolve substitution variables if the referenced value is available within this scope.
  * - Wrap unresolved substitution variables in a lazy reference that can later get
  *   resolved against a parent configuration, which may change when a document is re-homed.
  *
  * @author Jens Halm
  */
private[laika] object ConfigResolver {

  implicit class KeyOps(value: Key) {
    def name: String = value.segments.lastOption.getOrElse("")
  }

  /** Translates the configuration builder model (usually obtained from a HOCON parser)
    * into and partially resolved interim model.
    * It turns a root `ObjectBuilderValue` into a root `FieldRef`.
    * The only fields the interim model leaves unresolved are those that need to
    * query a fallback `Config`, since the fallback can change
    * when a `Document` gets "re-homed" into a different `DocumentTree`.
    *
    * The translation step involves the following steps:
    *
    * - Expand keys (e.g. `{ a.b.c = 7 }` will become `{ a = { b = { c = 7 }}}`)
    * - Merge objects with a common base path
    * - Merge concatenated values (e.g. `[1,2] [3,4]` will become `[1,2,3,4]`
    * - Resolve substitution variables if the referenced value is available within this scope.
    * - Wrap unresolved substitution variables in a lazy reference that can later get
    *   resolved against a parent configuration, which may change when a document is re-homed.
    */
  def resolve(
      root: ObjectBuilderValue,
      origin: Origin,
      includes: IncludeMap
  ): Either[ConfigError, FieldRef] = {

    val resolvedIncludes = resolveIncludes(root, includes)
    val errors           = extractErrors(resolvedIncludes)

    NonEmptyChain.fromSeq(errors) match {
      case Some(errs) => Left(InvalidFields(errs))
      case None       =>
        val rootExpanded = mergeObjects(expandPaths(resolvedIncludes))

        def resolveValue(key: Key)(value: ConfigBuilderValue): Option[FieldRef] = value match {
          case o: ObjectBuilderValue   => Some(resolveObject(key, o))
          case a: ArrayBuilderValue    =>
            val values = a.values.zipWithIndex.flatMap { case (value, index) =>
              resolveValue(Key(key.segments :+ index.toString))(value)
            }
            Some(FieldRef.arrayRef(key.name, origin, values.toList))
          case r: ResolvedBuilderValue => Some(ScalarRef(key.name, origin, r.value))
          case s: ValidStringValue     => Some(ScalarRef(key.name, origin, StringValue(s.value)))
          case c: ConcatValue          =>
            val parts = c.allParts.toList.flatMap(resolveConcatPart(key))
            Some(FieldRef.concatRef(key.name, origin, parts))
          case m: MergedValue          =>
            Some(FieldRef.mergedRef(key.name, origin, m.values.toList.flatMap(resolveValue(key))))
          case SubstitutionValue(ref, optional) if ref != key =>
            Some(resolveSubstitution(key, ref, optional).withName(key.name))
          case _                                              => None
        }

        def resolveObject(key: Key, value: ObjectBuilderValue): FieldRef = {
          val fields = value.values.toList.flatMap(fd => resolveValue(fd.validKey)(fd.value))
          FieldRef.objectRef(key.name, origin, fields)
        }

        def resolveSubstitution(parentKey: Key, refKey: Key, optional: Boolean): FieldRef = {
          if (parentKey.isChild(refKey))
            InvalidRef(refKey.name, origin, s"Circular Reference involving path '$refKey'")
          else
            FieldRef.substitution(parentKey.name, origin, refKey, optional)
        }

        def resolveConcatPart(key: Key)(part: ConcatPart): Option[FieldRef] = part.value match {
          case SelfReference => None
          case other         =>
            resolveValue(key)(other) match {
              case Some(scalar: ScalarRef) =>
                Some(scalar.copy(value = StringValue(part.whitespace + scalar.value.render)))
              case other                   => other
            }
        }

        Right(resolveObject(Key.root, rootExpanded))
    }
  }

  private def resolveIncludes(
      obj: ObjectBuilderValue,
      includes: IncludeMap
  ): ObjectBuilderValue = {

    val resolvedFields = obj.values.flatMap {
      case bf @ BuilderField(key, value @ IncludeBuilderValue(resource, source)) =>
        resource.resourceId match {
          case _: InvalidStringValue => Seq(bf) // errors get extracted later
          case _                     =>
            includes.get(resource) match {
              case Some(Left(error))           =>
                val msg = s"Error including '${resource.resourceId.value}': ${error.message}"
                Seq(
                  BuilderField(key, InvalidBuilderValue(value, Failure(Message.fixed(msg), source)))
                )
              case None if resource.isRequired =>
                val msg = s"Missing required include '${resource.resourceId.value}'"
                Seq(
                  BuilderField(key, InvalidBuilderValue(value, Failure(Message.fixed(msg), source)))
                )
              case None                        => Nil
              case Some(Right(included))       =>
                resolveIncludes(included, includes).values
            }
        }
      case BuilderField(key, nested: ObjectBuilderValue)                         =>
        Seq(BuilderField(key, resolveIncludes(nested, includes)))
      case other                                                                 => Seq(other)
    }
    ObjectBuilderValue(resolvedFields.sortBy(_.validKey.toString))
  }

  /** Merges objects with a common base path into a single one.
    *
    * {{{
    * a = { b = { c = 7 }}
    *
    * a = { b = { d = 9 }}
    * }}}
    *
    * will become
    *
    * {{{
    * a = { b = { c = 7, d = 9 }}
    * }}}
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
            case obv: ObjectBuilderValue => mergeObjects(obv)
            case other                   => other
          }
          BuilderField(key, merged)
      }
    ObjectBuilderValue(mergedFields)
  }

  /** Expands all flattened path expressions to nested objects.
    *
    * {{{
    * a.b.c = 7
    * }}}
    *
    * will become
    *
    * {{{
    * a = { b = { c = 7 }}
    * }}}
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
  def extractErrors(obj: ObjectBuilderValue, parentPath: String = ""): Seq[InvalidField] = {

    def extract(field: String, value: ConfigBuilderValue): Seq[InvalidField] = {
      def wrap(failure: Failure): Seq[InvalidField] = {
        val fieldName = if (parentPath.isEmpty) field else s"$parentPath.$field"
        Seq(InvalidField(fieldName, failure))
      }
      value match {
        case InvalidStringValue(_, failure)                          => wrap(failure)
        case InvalidBuilderValue(ArrayBuilderValue(values), failure) =>
          val nested = values.zipWithIndex.flatMap { case (arrValue, index) =>
            extract(field + "." + index, arrValue)
          }
          if (nested.isEmpty) wrap(failure) else nested
        case InvalidBuilderValue(obv: ObjectBuilderValue, failure)   =>
          val nested = extractErrors(obv)
          if (nested.isEmpty) wrap(failure) else nested
        case InvalidBuilderValue(_, failure)                         => wrap(failure)
        case incl: IncludeBuilderValue                               =>
          incl.resource.resourceId match {
            case InvalidStringValue(_, failure) => wrap(failure)
            case _                              => Nil
          }
        case child: ObjectBuilderValue                               =>
          val fieldName = if (parentPath.isEmpty) field else s"$parentPath.$field"
          extractErrors(child, fieldName)
        case child: ArrayBuilderValue                                =>
          child.values.zipWithIndex.flatMap { case (arrValue, index) =>
            extract(field + "." + index, arrValue)
          }
        case concat: ConcatValue                                     =>
          (concat.first +: concat.rest.map(_.value)).flatMap(extract(field, _))
        case _                                                       => Nil
      }
    }

    obj.values.flatMap {
      case BuilderField(Left(InvalidStringValue(_, failure)), inv: InvalidBuilderValue)
          if failure.message.contains("unquoted string") =>
        // keep the message of the value failure with the position of the key failure for more clarity
        Seq(InvalidField("<invalid>", failure.copy(msgProvider = inv.failure.msgProvider)))
      case BuilderField(Left(InvalidStringValue(_, failure)), _) =>
        Seq(InvalidField("<invalid>", failure))
      case BuilderField(name, value) => extract(name.map(_.toString).getOrElse("<invalid>"), value)
    }
  }

}
