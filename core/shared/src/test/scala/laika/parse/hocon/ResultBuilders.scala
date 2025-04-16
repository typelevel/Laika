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

import laika.api.config.ConfigValue.*
import laika.internal.parse.hocon.*
import laika.parse.{ LineSource, SourceCursor, SourceFragment }

/** @author Jens Halm
  */
trait ResultBuilders {

  def nullValue(input: String): ConfigBuilderValue =
    ResolvedBuilderValue(NullValue, cursor("null", input))

  def trueValue(input: String): ConfigBuilderValue =
    ResolvedBuilderValue(BooleanValue(true), cursor("true", input))

  def falseValue(input: String): ConfigBuilderValue =
    ResolvedBuilderValue(BooleanValue(false), cursor("false", input))

  def longValue(value: Long, input: String): ConfigBuilderValue =
    ResolvedBuilderValue(LongValue(value), cursor(value.toString, input))

  def doubleValue(value: Double, input: String): ConfigBuilderValue =
    ResolvedBuilderValue(DoubleValue(value), cursor(value.toString, input))

  def stringValue(value: String, input: String): ConfigBuilderValue =
    ResolvedBuilderValue(StringValue(value), cursor(value, input))

  def cursor(value: String): SourceFragment = cursor(value, value)

  def cursor(value: String, input: String): SourceFragment = {
    val index = input.indexOf(value)
    LineSource(value, SourceCursor(input).consume(index))
  }

}
