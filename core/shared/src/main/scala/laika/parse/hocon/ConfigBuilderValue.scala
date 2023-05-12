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

import laika.config.{ Key, SimpleConfigValue }
import laika.parse.Failure

/** The base trait of the interim configuration model (usually obtained from a HOCON parser).
  *
  * This type is not exposed to public APIs as it will be translated to a final object
  * structure later. It contains instances representing interim constructs like concatenated
  * values or substitution variables which will not be present in the final model.
  *
  * @author Jens Halm
  */
sealed trait ConfigBuilderValue extends Product with Serializable

/** A concatenated value (either all objects, all arrays, all simple values or invalid). */
case class ConcatValue(first: ConfigBuilderValue, rest: Seq[ConcatPart])
    extends ConfigBuilderValue {
  val allParts: Seq[ConcatPart] = ConcatPart("", first) +: rest
}

/** A single part of a concatenated value with the whitespace between this and the previous value preserved. */
case class ConcatPart(whitespace: String, value: ConfigBuilderValue)

/** A merged value with "last one wins" semantics for the provided values (objects will be merged instead). */
case class MergedValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue

/** A substitution reference that may be marked as optional. */
case class SubstitutionValue(ref: Key, optional: Boolean) extends ConfigBuilderValue

object SubstitutionValue {
  def apply(ref: String, optional: Boolean): SubstitutionValue = apply(Key(ref), optional)
}

sealed trait StringBuilderValue extends ConfigBuilderValue {
  def value: String
}

case class ValidStringValue(value: String)                     extends StringBuilderValue
case class InvalidStringValue(value: String, failure: Failure) extends StringBuilderValue

/** A marker for a self reference, a reference to an earlier definition with the same key. */
case object SelfReference extends ConfigBuilderValue

/** An array value with all its elements. */
case class ArrayBuilderValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue

/** An object value with all its fields. */
case class ObjectBuilderValue(values: Seq[BuilderField]) extends ConfigBuilderValue

/** A single field of an object value. */
case class BuilderField(key: Either[InvalidStringValue, Key], value: ConfigBuilderValue) {
  def validKey: Key = key.getOrElse(Key.root)
}

object BuilderField {
  def apply(key: String, value: ConfigBuilderValue): BuilderField = apply(Right(Key(key)), value)
  def apply(key: Key, value: ConfigBuilderValue): BuilderField    = apply(Right(key), value)
}

case class InvalidBuilderValue(value: ConfigBuilderValue, failure: Failure)
    extends ConfigBuilderValue

/** A simple configuration value that does not need to be recursively resolved. */
case class ResolvedBuilderValue(value: SimpleConfigValue) extends ConfigBuilderValue

/** Description of a resource to be included in the current configuration. */
sealed trait IncludeResource {
  def resourceId: StringBuilderValue
  def isRequired: Boolean

  def asRequired: IncludeResource = this match {
    case i: IncludeUrl       => i.copy(isRequired = true)
    case i: IncludeFile      => i.copy(isRequired = true)
    case i: IncludeClassPath => i.copy(isRequired = true)
    case i: IncludeAny       => i.copy(isRequired = true)
  }

}

case class IncludeUrl(resourceId: StringBuilderValue, isRequired: Boolean = false)
    extends IncludeResource

case class IncludeFile(resourceId: StringBuilderValue, isRequired: Boolean = false)
    extends IncludeResource

case class IncludeClassPath(resourceId: StringBuilderValue, isRequired: Boolean = false)
    extends IncludeResource

case class IncludeAny(resourceId: StringBuilderValue, isRequired: Boolean = false)
    extends IncludeResource

case class IncludeBuilderValue(resource: IncludeResource) extends ConfigBuilderValue
