/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.parse.helper

import org.scalactic.{Prettifier, source}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MigrationSpec extends AnyWordSpec with Matchers {

  def assertEquals (actual: Any, expected: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = 
    assertResult(expected)(actual)
  
}

class MigrationFlatSpec extends AnyFlatSpec with Matchers {

  def assertEquals (actual: Any, expected: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assertResult(expected)(actual)

}
