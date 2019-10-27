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
import laika.config.SimpleConfigValue

/** The base trait of the interim configuration model (usually obtained from a HOCON parser).
  * 
  * This type is not exposed to public APIs as it will be translated to a final object
  * structure later. It contains instances representing interim constructs like concatenated
  * values or substitution variables which will not be present in the final model.
  * 
  * @author Jens Halm
  */
sealed trait ConfigBuilderValue

/** A concatenated value (either all objects, all arrays, all simple values or invalid). */
case class ConcatValue(first: ConfigBuilderValue, rest: Seq[ConcatPart]) extends ConfigBuilderValue {
  val allParts: Seq[ConcatPart] = ConcatPart("", first) +: rest
}

/** A single part of a concatenated value with the whitespace between this and the previous value preserved. */
case class ConcatPart(whitespace: String, value: ConfigBuilderValue)

/** A merged value with "last one wins" semantics for the provided values (objects will be merged instead). */
case class MergedValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue

/** A substitution reference that may be marked as optional. */
case class SubstitutionValue(ref: Path, optional: Boolean) extends ConfigBuilderValue
object SubstitutionValue {
  def apply (ref: String, optional: Boolean): SubstitutionValue = apply(Path.Root / ref, optional)
}

/** A marker for a self reference, a reference to an earlier definition with the same key. */
case object SelfReference extends ConfigBuilderValue

/** An array value with all its elements. */
case class ArrayBuilderValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue

/** An object value with all its fields. */
case class ObjectBuilderValue(values: Seq[BuilderField]) extends ConfigBuilderValue

/** A single field of an object value. */
case class BuilderField(key: Path, value: ConfigBuilderValue)
object BuilderField {
  def apply (key: String, value: ConfigBuilderValue): BuilderField = apply(Path.Root / key, value)
}

/** A simple configuration value that does not need to be recursively resolved. */
case class ResolvedBuilderValue(value: SimpleConfigValue) extends ConfigBuilderValue
