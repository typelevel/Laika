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

package laika.parse.text

import cats.implicits._
import cats.data.NonEmptySet
import laika.parse.{Failure, Parser}
import laika.parse.text.TextParsers._
import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class DelimiterParserSpec extends WordSpec with Matchers with ParseResultHelpers with StringParserHelpers {

  val skip: Parser[Unit] = any.take(1).^
  
  val abc: NonEmptySet[Char] = NonEmptySet.of('a', 'b', 'c')
  
  "The delimiter parser" should {
    
    "parse a character delimiter" in {
      Parsing ("*") using delimiter('*') should produce("*") 
    }

    "parse a string literal delimiter" in {
      Parsing ("**") using delimiter("**") should produce("**")
    }

    "fail if a string literal delimiter is not matched" in {
      Parsing ("*") using delimiter("**") should cause[Failure]
    }

    "parse a character group delimiter" in {
      Parsing ("cb") using delimiter(prefix(abc).take(2)) should produce("cb")
    }

    "fail if a character group delimiter is not matched" in {
      Parsing ("cd") using delimiter(prefix(abc).take(2)) should cause[Failure]
    }

    "parse a delimiter with a character post-condition" in {
      Parsing ("**:") using delimiter("**").nextNot('.') should produce("**")
    }

    "fail when a character post-condition is not satisfied" in {
      Parsing ("**.") using delimiter("**").nextNot('.') should cause[Failure]
    }

    "parse a delimiter with a character set post-condition" in {
      Parsing ("**:") using delimiter("**").nextNot(abc) should produce("**")
    }

    "fail when a character set post-condition is not satisfied" in {
      Parsing ("**a") using delimiter("**").nextNot(abc) should cause[Failure]
    }

    "parse a delimiter with a post-condition function" in {
      Parsing ("**:") using delimiter("**").nextNot(_ > 'a') should produce("**")
    }

    "fail when a post-condition function is not satisfied" in {
      Parsing ("**z") using delimiter("**").nextNot(_ > 'a') should cause[Failure]
    }

    "parse a delimiter with a character pre-condition" in {
      Parsing (":**") using skip ~> delimiter("**").prevNot('.') should produce("**")
    }

    "fail when a character pre-condition is not satisfied" in {
      Parsing (".**") using skip ~> delimiter("**").prevNot('.') should cause[Failure]
    }

    "parse a delimiter with a character set pre-condition" in {
      Parsing (":**") using skip ~> delimiter("**").prevNot(abc) should produce("**")
    }

    "fail when a character set pre-condition is not satisfied" in {
      Parsing ("a**") using skip ~> delimiter("**").prevNot(abc) should cause[Failure]
    }

    "parse a delimiter with a pre-condition function" in {
      Parsing (":**") using skip ~> delimiter("**").prevNot(_ > 'a') should produce("**")
    }

    "fail when a pre-condition function is not satisfied" in {
      Parsing ("z**") using skip ~> delimiter("**").prevNot(_ > 'a') should cause[Failure]
    }
    
    "allow composing of two delimiters" in {
      val inner = delimiter("*").nextNot('a')
      val outer = delimiter(inner).nextNot('b')
      Parsing ("*a") using outer should cause[Failure]
      Parsing ("*b") using outer should cause[Failure]
      Parsing ("*c") using outer should produce("*")
    }
    
  }
  
}
