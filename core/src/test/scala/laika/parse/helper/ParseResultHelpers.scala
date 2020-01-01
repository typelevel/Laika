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

package laika.parse.helper

import laika.ast.{Element, ElementContainer}
import laika.parse.{Failure, Parsed, Success}
import org.scalatest.matchers.{MatchResult, Matcher}

trait ParseResultHelpers {

  class ParserFailureMatcher[T <: Failure](m: Manifest[T]) extends Matcher[Parsed[_]] {

    def apply (left: Parsed[_]): MatchResult = {

      val failureMessageSuffix = left match {
        case Success(result,_)  => s"parser produced result $result instead of failing with result type ${m.runtimeClass.getSimpleName}"
        case f @ Failure(_,_,_) => s"parser result type ${f.getClass.getSimpleName} was not the expected type ${m.runtimeClass.getSimpleName}"
      }
      val negatedFailureMessageSuffix = s"parser '$left' did have the unexpected result type ${m.runtimeClass.getSimpleName}"

      MatchResult(
        left.isFailure && m.runtimeClass.isInstance(left),
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }

  def cause [T <: Failure] (implicit m: Manifest[T]): ParserFailureMatcher[T] = new ParserFailureMatcher[T](m)
  
  class ParserSuccessMatcher[T] (expected: T) extends Matcher[Parsed[T]] {

    def apply (left: Parsed[T]): MatchResult = {

      val failureMessageSuffix = left match {
        case Success(unexpected,_) => s"parser result '$unexpected' was not equal to '$expected'"
        case Failure(msg,in,_)     => s"parser failed with message '${msg.message(in)}' at ${in.position} instead of producing expected result '$expected'"
      }
      
      val negatedFailureMessageSuffix = s"parser '$left' did produce the unexpected result $expected"

      MatchResult(
        left.toOption.contains(expected),
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }
  
  def produce [T] (result: T): ParserSuccessMatcher[T] = new ParserSuccessMatcher(result)

  class ParserContainsMatcher (expected: Element) extends Matcher[Parsed[ElementContainer[_]]] {

    def apply (left: Parsed[ElementContainer[_]]): MatchResult = {

      val failureMessageSuffix = left match {
        case Success(unexpected,_) => s"parser result '$unexpected' did not contain '$expected'"
        case Failure(msg,in,_)     => s"parser failed with message '${msg.message(in)}' at ${in.position} instead of producing expected result containing '$expected'"
      }

      val negatedFailureMessageSuffix = s"parser '$left' did produce the unexpected result $expected"

      MatchResult(
        left.toOption.exists(_.select(_ == expected).nonEmpty),
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }

  def containElement (element: Element): ParserContainsMatcher = new ParserContainsMatcher(element)
  
}
