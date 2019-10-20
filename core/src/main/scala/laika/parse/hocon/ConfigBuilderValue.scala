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

/**
  * @author Jens Halm
  */
sealed trait ConfigBuilderValue

case class ConcatValue(first: ConfigBuilderValue, rest: Seq[ConcatPart]) extends ConfigBuilderValue {
  val allParts: Seq[ConcatPart] = ConcatPart("", first) +: rest
}
case class MergedValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue
case class ConcatPart(whitespace: String, value: ConfigBuilderValue)
case class SubstitutionValue(ref: Path, optional: Boolean) extends ConfigBuilderValue
object SubstitutionValue {
  def apply (ref: String, optional: Boolean): SubstitutionValue = apply(Path.Root / ref, optional)
}
case object SelfReference extends ConfigBuilderValue
case class ArrayBuilderValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue
case class ObjectBuilderValue(values: Seq[BuilderField]) extends ConfigBuilderValue
case class BuilderField(key: Path, value: ConfigBuilderValue)
object BuilderField {
  def apply (key: String, value: ConfigBuilderValue): BuilderField = apply(Path.Root / key, value)
}
case class ResolvedBuilderValue(value: SimpleConfigValue) extends ConfigBuilderValue
