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

package laika.config

/** A type class that can encode a value of type T as a ConfigValue.
  * 
  * @author Jens Halm
  */
trait ConfigEncoder[-T] {
  def apply(value: T): ConfigValue
}

/** Companion containing default encoder implementations for simple values and Seq's.
  */
object ConfigEncoder {

  implicit val string: ConfigEncoder[String] = new ConfigEncoder[String] {
    def apply (value: String) = StringValue(value)
  }

  implicit val int: ConfigEncoder[Int] = new ConfigEncoder[Int] {
    def apply (value: Int) = LongValue(value.toLong)
  }

  implicit val double: ConfigEncoder[Double] = new ConfigEncoder[Double] {
    def apply (value: Double) = DoubleValue(value)
  }

  implicit val configValue: ConfigEncoder[ConfigValue] = new ConfigEncoder[ConfigValue] {
    def apply (value: ConfigValue) = value
  }

  implicit def seq[T] (implicit elementEncoder: ConfigEncoder[T]): ConfigEncoder[Seq[T]] = new ConfigEncoder[Seq[T]] {
    def apply (value: Seq[T]) = ArrayValue(value.map(elementEncoder.apply))
  }
}
