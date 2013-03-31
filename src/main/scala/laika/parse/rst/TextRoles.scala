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

package laika.parse.rst

import laika.tree.Elements._
import laika.util.Builders._

/** API for creating interpreted text roles, the extension mechanism for inline elements of reStructuredText.
 *  The API did not aim to mimic the API of the original Python reference implementation.
 *  Instead the goal was to create an API that is idiomatic Scala, fully typesafe and as concise as possible.
 *  Yet it should be flexible enough to semantically support the options of the Python text roles, so that
 *  ideally most existing Python text roles could theoretically get ported to Laika.
 * 
 *  Entry points is the `TextRole` object. 
 * 
 *  TODO - examples for directive setup
 * 
 *  @author Jens Halm
 */
object TextRoles {

  
  trait RoleDirectiveParser {
      
    def requiredField [T](name: String, f: String => Either[String,T]): Result[T]
    
    def optionalField [T](name: String, f: String => Either[String,T]): Result[Option[T]]
    
    def standardContent: Result[Seq[Block]]
    
    def content [T](f: String => Either[String,T]): Result[T]
    
  }
  
  abstract class RoleDirectivePart[+A] extends (RoleDirectiveParser => Result[A]) { self =>
    
    def map [B](f: A => B) = new RoleDirectivePart[B] { 
      def apply (p: RoleDirectiveParser) = self(p) map f 
    }
    
  }
  
  implicit object CanBuildRoleDirectivePart extends CanBuild[RoleDirectivePart] {
    
    def apply [A,B](ma: RoleDirectivePart[A], mb: RoleDirectivePart[B]) = new RoleDirectivePart[A~B] {
      def apply (p: RoleDirectiveParser) = {
        val a = ma(p)
        val b = mb(p)
        new Result(new ~(a.get,b.get))
      }
    }
  
    def map [A,B](m: RoleDirectivePart[A], f: A => B) = m map f
    
  }
 
  object Parts {
    
    private def part [T](f: RoleDirectiveParser => Result[T]) = new RoleDirectivePart[T] {
      def apply (p: RoleDirectiveParser) = f(p)
    }
    
    def requiredField [T](name: String): RoleDirectivePart[String] = part(_.requiredField(name, s => Right(s)))
    
    def requiredField [T](name: String, f: String => Either[String,T]): RoleDirectivePart[T] = part(_.requiredField(name, f))
    
    def optionalField (name: String): RoleDirectivePart[Option[String]] = part(_.optionalField(name, s => Right(s)))
    
    def optionalField [T](name: String, f: String => Either[String,T]): RoleDirectivePart[Option[T]] = part(_.optionalField(name, f))
    
    def standardContent: RoleDirectivePart[Seq[Block]] = part(_.standardContent) // TODO - maybe use blockContent/spanContent
    
    def content [T](f: String => Either[String,T]): RoleDirectivePart[T] = part(_.content(f))
    
  }

  
  class TextRole private (val name: String, val part: RoleDirectivePart[String => Seq[Span]])

  object TextRole {
    
    def apply [T] (name: String, default: T)(part: RoleDirectivePart[T])(roleF: (T, String) => Seq[Span]) = 
      new TextRole(name, part map (res => (str => roleF(res, str))))
    
  }

  
}







