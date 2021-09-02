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

import laika.config.{BooleanValue, DoubleValue, LongValue, NullValue}

/**
  * @author Jens Halm
  */
trait ResultBuilders {

  val nullValue: ConfigBuilderValue = ResolvedBuilderValue(NullValue)
  val trueValue: ConfigBuilderValue = ResolvedBuilderValue(BooleanValue(true))
  val falseValue: ConfigBuilderValue = ResolvedBuilderValue(BooleanValue(false))
  def longValue(value: Long): ConfigBuilderValue = ResolvedBuilderValue(LongValue(value))
  def doubleValue(value: Double): ConfigBuilderValue = ResolvedBuilderValue(DoubleValue(value))
  def stringValue(value: String): ConfigBuilderValue = ValidStringValue(value)
  
}
