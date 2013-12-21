/*
 * Copyright 2013 the original author or authors.
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

import scala.util.parsing.combinator.Parsers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait ParseResultHelpers { self: Parsers =>

  class ParserNoSuccessMatcher[T <: NoSuccess] (m: Manifest[T]) extends Matcher[ParseResult[_]] {

    def apply (left: ParseResult[_]) = {

      val failureMessageSuffix = left match {
        case Success(result,_)   => s"parser produced result $result instead of failing with result type ${m.runtimeClass.getSimpleName}"
        case ns @ NoSuccess(_,_) => s"parser result type ${ns.getClass.getSimpleName} was not the expected type ${m.runtimeClass.getSimpleName}"
      }
      val negatedFailureMessageSuffix = s"parser '$left' did have the unexpected result type ${m.runtimeClass.getSimpleName}"

      MatchResult(
        left.isEmpty && m.runtimeClass.isInstance(left),
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }
  
  def cause [T <: NoSuccess] (implicit m: Manifest[T]) = new ParserNoSuccessMatcher[T](m)
  
  class ParserSuccessMatcher[T] (expected: T) extends Matcher[ParseResult[T]] {

    def apply (left: ParseResult[T]) = {

      val failureMessageSuffix = left match {
        case Success(unexpected,_) => s"parser result '$unexpected' was not equal to '$expected'"
        case NoSuccess(msg,in)     => s"parser failed with message '$msg' at ${in.pos} instead of producing expected result '$expected'"
      }
      
      val negatedFailureMessageSuffix = s"parser '$left' did produce the unexpected result $expected"

      MatchResult(
        left.successful && left.get == expected,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }
  
  def produce [T] (result: T) = new ParserSuccessMatcher(result)
  
  
}