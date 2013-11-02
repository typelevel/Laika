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

package laika.directive

import laika.util.Builders._
import laika.tree.Elements._
import laika.tree.Documents.DocumentContext
import laika.tree.Templates.TemplateSpan

/** 
 *  @author Jens Halm
 */
object Directives {

  
  sealed abstract class Result[+A] {
    
    def get: A
    
    def map [B](f: A => B): Result[B]
    
    def flatMap [B](f: A => Result[B]): Result[B]
    
    def ~ [B](result: Result[B]): Result[A ~ B]
    
  }
  
  case class Success[+A] (a: A) extends Result[A] {
    
    def get = a
    
    def map [B](f: A => B) = Success(f(get))
    
    def flatMap [B](f: A => Result[B]) = f(a)
    
    def ~ [B](result: Result[B]) = result match {
      case Success(value) => Success(new ~(a, value))
      case Failure(msg)   => Failure(msg)
    }
    
  }
  
  case class Failure (messages: Seq[String]) extends Result[Nothing] {
    
    def get = throw new RuntimeException("no result as processing failed")
    
    def map [B](f: Nothing => B) = this
    
    def flatMap [B](f: Nothing => Result[B]) = this
    
    def ~ [B](result: Result[B]) = result match {
      case Success(value)     => this
      case Failure(otherMsg)  => Failure(messages ++ otherMsg)
    }
    
  }
  
  object Failure {
    def apply (msg: String) = new Failure(Seq(msg))
  }
  
  sealed abstract class Id {
    def desc (keyType: String): String
  }
  
  case class Named (name: String) extends Id {
    def desc (keyType: String) = keyType+" with name '"+name+"'"
  }
  
  case object Default extends Id {
    def desc (keyType: String) = "default "+keyType
  }
  
  implicit def stringToId (str: String): Id = Named(str)
  

  sealed abstract class Key (keyType: String) {
    def id: Id
    def desc = id.desc(keyType)
  }
  
  case class Attribute (id: Id) extends Key("attribute")

  case class Body (id: Id) extends Key("body")
  
  
  
  trait BuilderContext[E <: Element] {
  
    type Parser <: (String => Seq[E])
    
    trait DirectiveContext {
        
      def part (key: Key): Option[String]
      
      def context: Option[DocumentContext]
      
      def parser: Parser
  
    }
    
    abstract class DirectivePart[+A] extends (DirectiveContext => Result[A]) { self =>
      
      def map [B](f: A => B) = new DirectivePart[B] { 
        def apply (p: DirectiveContext) = self(p) map f 
        def requiresContext = self.requiresContext
      }
      
      def requiresContext: Boolean
      
      def optional: DirectivePart[Option[A]] = map (Some(_))
      
    }
    
    implicit object CanBuildDirectivePart extends CanBuild[DirectivePart] {
      
      def apply [A,B](ma: DirectivePart[A], mb: DirectivePart[B]) = new DirectivePart[A~B] {
        def apply (p: DirectiveContext) = ma(p) ~ mb(p)
        def requiresContext = ma.requiresContext || mb.requiresContext
      }
      
      def map [A,B](m: DirectivePart[A], f: A => B) = m map f
      
    }
    
    type Converter[T] = (Parser, String) => Result[T]
    
    object Converters {
      
      val string = (p: Parser, s: String) => Success(s)
      
      val parsed = (p: Parser, s: String) => Success(p(s))
      
      val int = (p: Parser, s: String) => toInt(s, _ => true)

      val positiveInt = (p: Parser, s: String) => toInt(s, _ > 0, "Not a positive integer")

      val nonNegativeInt = (p: Parser, s: String) => toInt(s, _ >= 0, "Not a non-negative integer")
      
      private def toInt (s: String, f: Int => Boolean, msg: String = "") = { 
        try { 
          val i = s.toInt
          if (f(i)) Success(i) else Failure(msg + ": " + i)
        } catch { 
          case e: NumberFormatException => Failure("Not an integer: " + s)
        }
    }
      
    }
   
    object Combinators {
      
      private def requiredPart [T] (key: Key, converter: Converter[T], msg: => String) = new DirectivePart[T] {
      
        val requiresContext = false
        
        def convert (context: DirectiveContext) = context.part(key).map(s => converter(context.parser, s))
        
        def apply (context: DirectiveContext) = convert(context).getOrElse(Failure(Seq(msg)))
        
        override def optional = new DirectivePart[Option[T]] {
          val requiresContext = false
          def apply (context: DirectiveContext) = convert(context) match {
            case Some(Success(value)) => Success(Some(value))
            case Some(Failure(msg))   => Failure(msg)
            case None                 => Success(None)
          }
        }
        
      }
      
      private def part [T](f: DirectiveContext => Result[T], reqContext: Boolean = false) = new DirectivePart[T] {
        def apply (p: DirectiveContext) = f(p)
        val requiresContext = reqContext
      }
      
      def attribute [T](id: Id, converter: Converter[T] = Converters.string): DirectivePart[T] 
          = requiredPart(Attribute(id), converter, "required "+Attribute(id).desc+" is missing") 
      
      def body [T](id: Id, converter: Converter[T] = Converters.parsed): DirectivePart[T] 
          = requiredPart(Body(id), converter, "required "+Body(id).desc+" is missing")
      
      def parser: DirectivePart[Parser] = part(c => Success(c.parser))

      def context: DirectivePart[DocumentContext] 
          = part(_.context map (Success(_)) getOrElse (Failure("DocumentContext not available yet")), true)
      
    }
  
    class Directive private[Directives] (val name: String, part: DirectivePart[E]) {
      def apply (context: DirectiveContext):Result[E] = part(context)
      def requiresContext = part.requiresContext
    }
    
    def create (name: String)(part: DirectivePart[E]) = new Directive(name.toLowerCase, part)
    
    def toMap (directives: Traversable[Directive]) = directives map (dir => (dir.name, dir)) toMap
    
  }
  
  object Spans extends BuilderContext[Span] {
    
    trait Parser extends (String => Seq[Span]) {
      def apply (source: String): Seq[Span]
    }
    
  }
  
  object Blocks extends BuilderContext[Block] {

    trait Parser extends (String => Seq[Block]) {
      def apply (source: String): Seq[Block]
      def parseInline (source: String): Seq[Span]
    }
    
  }
  
  object Templates extends BuilderContext[TemplateSpan] {

    trait Parser extends (String => Seq[TemplateSpan]) {
      def apply (source: String): Seq[TemplateSpan]
    }
    
  }
  
  
}