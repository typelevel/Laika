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

import laika.config.ConfigParser
import laika.io.FileIO
import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class HoconParserIntegrationSpec extends AnyWordSpec with Matchers with ParseResultHelpers with StringParserHelpers with ResultBuilders {

  "The root parser" should {

    "successfully parse the full Akka default configuration" in {
      val input = FileIO.readFile(FileIO.classPathResourcePath("/akka.conf"))
      ConfigParser.parse(input).resolve().isRight shouldBe true
    }

  }

}
