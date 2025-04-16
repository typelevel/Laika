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

import laika.api.config.ConfigValue.SimpleValue
import laika.api.config.Key
import laika.parse.{ Failure, SourceCursor, SourceFragment }

/** The base trait of the interim configuration model (usually obtained from a HOCON parser).
  *
  * This type is not exposed to public APIs as it will be translated to a final object
  * structure later. It contains instances representing interim constructs like concatenated
  * values or substitution variables which will not be present in the final model.
  *
  * @author Jens Halm
  */
private[laika] sealed trait ConfigBuilderValue extends Product with Serializable

/** A concatenated value (either all objects, all arrays, all simple values or invalid). */
private[laika] case class ConcatValue(first: ConfigBuilderValue, rest: Seq[ConcatPart])
    extends ConfigBuilderValue {
  val allParts: Seq[ConcatPart] = ConcatPart("", first) +: rest
}

/** A single part of a concatenated value with the whitespace between this and the previous value preserved. */
private[laika] case class ConcatPart(whitespace: String, value: ConfigBuilderValue)

/** A merged value with "last one wins" semantics for the provided values (objects will be merged instead). */
private[laika] case class MergedValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue

/** A substitution reference that may be marked as optional. */
private[laika] case class SubstitutionValue(ref: Key, optional: Boolean) extends ConfigBuilderValue

private[laika] object SubstitutionValue {
  def apply(ref: String, optional: Boolean): SubstitutionValue = apply(Key(ref), optional)
}

private[laika] sealed trait ParsedString {
  def value: String
}

private[laika] case class ValidString(value: String, source: SourceFragment) extends ParsedString

private[laika] case class InvalidString(value: String, failure: Failure) extends ParsedString

/** A marker for a self reference, a reference to an earlier definition with the same key. */
private[laika] case object SelfReference extends ConfigBuilderValue

/** An array value with all its elements. */
private[laika] case class ArrayBuilderValue(values: Seq[ConfigBuilderValue])
    extends ConfigBuilderValue

/** An object value with all its fields. */
private[laika] case class ObjectBuilderValue(values: Seq[BuilderField]) extends ConfigBuilderValue

/** A single field of an object value. */
private[laika] case class BuilderField(
    key: Either[InvalidString, Key],
    value: ConfigBuilderValue
) {
  def validKey: Key = key.getOrElse(Key.root)
}

private[laika] object BuilderField {
  def apply(key: String, value: ConfigBuilderValue): BuilderField = apply(Right(Key(key)), value)
  def apply(key: Key, value: ConfigBuilderValue): BuilderField    = apply(Right(key), value)
}

private[laika] case class InvalidBuilderValue(value: ConfigBuilderValue, failure: Failure)
    extends ConfigBuilderValue

/** A simple configuration value that does not need to be recursively resolved. */
private[laika] case class ResolvedBuilderValue(value: SimpleValue, source: SourceFragment)
    extends ConfigBuilderValue

/** Description of a resource to be included in the current configuration. */
private[laika] sealed trait IncludeResource {
  def resourceId: ParsedString
  def isRequired: Boolean

  def asRequired: IncludeResource = this match {
    case i: IncludeUrl       => i.copy(isRequired = true)
    case i: IncludeFile      => i.copy(isRequired = true)
    case i: IncludeClassPath => i.copy(isRequired = true)
    case i: IncludeAny       => i.copy(isRequired = true)
  }

}

private[laika] case class IncludeUrl(resourceId: ParsedString, isRequired: Boolean = false)
    extends IncludeResource

private[laika] case class IncludeFile(resourceId: ParsedString, isRequired: Boolean = false)
    extends IncludeResource

private[laika] case class IncludeClassPath(
    resourceId: ParsedString,
    isRequired: Boolean = false
) extends IncludeResource

private[laika] case class IncludeAny(resourceId: ParsedString, isRequired: Boolean = false)
    extends IncludeResource

private[laika] case class IncludeBuilderValue(resource: IncludeResource, cursor: SourceCursor)
    extends ConfigBuilderValue
