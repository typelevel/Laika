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

import cats.data.NonEmptySet
import cats.implicits._
import laika.parse.Parser
import laika.parse.builders._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class DelimiterParserSpec extends AnyWordSpec with Matchers {

  val skip: Parser[Unit] = oneChar.void
  
  val abc: NonEmptySet[Char] = NonEmptySet.of('a', 'b', 'c')
  
  "The delimiter parser" should {
    
    "parse a character delimiter" in {
      delimiter('*').parse("*").toEither shouldBe Right("*") 
    }

    "parse a string literal delimiter" in {
      delimiter("**").parse("**").toEither shouldBe Right("**")
    }

    "fail if a string literal delimiter is not matched" in {
      delimiter("**").parse("*").toEither.isLeft shouldBe true
    }

    "parse a character group delimiter" in {
      delimiter(someOf(abc).take(2)).parse("cb").toEither shouldBe Right("cb")
    }

    "fail if a character group delimiter is not matched" in {
      delimiter(someOf(abc).take(2)).parse("cd").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a character post-condition" in {
      delimiter("**").nextNot('.').parse("**:").toEither shouldBe Right("**")
    }

    "fail when a character post-condition is not satisfied" in {
      delimiter("**").nextNot('.').parse("**.").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a character set post-condition" in {
      delimiter("**").nextNot(abc).parse("**:").toEither shouldBe Right("**")
    }

    "fail when a character set post-condition is not satisfied" in {
      delimiter("**").nextNot(abc).parse("**a").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a post-condition function" in {
      delimiter("**").nextNot(_ > 'a').parse("**:").toEither shouldBe Right("**")
    }

    "fail when a post-condition function is not satisfied" in {
      delimiter("**").nextNot(_ > 'a').parse("**z").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a character pre-condition" in {
      (skip ~> delimiter("**").prevNot('.')).parse(":**").toEither shouldBe Right("**")
    }

    "fail when a character pre-condition is not satisfied" in {
      (skip ~> delimiter("**").prevNot('.')).parse(".**").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a character set pre-condition" in {
      (skip ~> delimiter("**").prevNot(abc)).parse(":**").toEither shouldBe Right("**")
    }

    "fail when a character set pre-condition is not satisfied" in {
      (skip ~> delimiter("**").prevNot(abc)).parse("a**").toEither.isLeft shouldBe true
    }

    "parse a delimiter with a pre-condition function" in {
      (skip ~> delimiter("**").prevNot(_ > 'a')).parse(":**").toEither shouldBe Right("**")
    }

    "fail when a pre-condition function is not satisfied" in {
      (skip ~> delimiter("**").prevNot(_ > 'a')).parse("z**").toEither.isLeft shouldBe true
    }
    
    "allow composing of two delimiters" in {
      val inner = delimiter("*").nextNot('a')
      val outer = delimiter(inner).nextNot('b')
      
      outer.parse("*a").toEither.isLeft shouldBe true
      outer.parse("*b").toEither.isLeft shouldBe true
      outer.parse("*c").toEither shouldBe Right("*")
    }
    
  }
  
}
