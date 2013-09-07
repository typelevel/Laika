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

import Directives._
import laika.tree.Elements._
import laika.util.Builders._
import scala.collection.mutable.ListBuffer
import laika.tree.Documents.DocumentContext

/** 
 * @author Jens Halm
 */
trait DirectiveParsers extends laika.parse.BlockParsers with laika.parse.InlineParsers { 
  
  
  def escapedUntil (char: Char) = escapedText(anyUntil(char) min 1)

  def escapedText (p: TextParser) = text(p, Map('\\' -> (any take 1))) // TODO - should be promoted to generic inline parser
  
  
  def blockDirective (name: String): Option[Blocks.Directive]
  
  def spanDirective (name: String): Option[Spans.Directive]
  
  
  case class Part (key: Key, content: String)

  case class ParsedDirective (name: String, parts: List[Part])
  
  type PartMap = Map[Key, String]
  
  
  lazy val wsOrNl = anyOf(' ','\t', '\n')
  
  lazy val refName: Parser[String] = (anyIn('a' to 'z', 'A' to 'Z') take 1) ~ 
    anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '-', '_') ^^ { case first ~ rest => first + rest }
  
  
  lazy val attrName: Parser[String] = refName <~ wsOrNl ~ '=' ~ wsOrNl
      
  lazy val attrValue: Parser[String] = anyBut(' ','\t','\n','.',':') | '"' ~> escapedUntil('"') 

  lazy val defaultAttribute: Parser[Part] = not(attrName) ~> attrValue ^^ { Part(Attribute(Default), _) }

  lazy val attribute: Parser[Part] = attrName ~ attrValue ^^ { case name ~ value => Part(Attribute(name), value) }
 

  lazy val declaration: Parser[(String, List[Part])] 
    = ("@" ~> refName <~ wsOrNl) ~ opt(defaultAttribute <~ wsOrNl) ~ ((wsOrNl ~> attribute)*) <~ ws ^^ 
      { case name ~ defAttr ~ attrs => (name, defAttr.toList ::: attrs) }

  
  lazy val bodyName: Parser[String] = '~' ~> refName <~ ws ~ ':'
  
  lazy val noBody = '.' ^^^ List[Part]()
  
  def directiveParser (bodyContent: Parser[String]): Parser[ParsedDirective] = {
    
    val defaultBody: Parser[Part] = not(wsOrNl ~> bodyName) ~> bodyContent ^^ { Part(Body(Default),_) }
    
    val body: Parser[Part] = wsOrNl ~> bodyName ~ bodyContent ^^ { case name ~ content => Part(Body(name), content) }
    
    val bodies = ':' ~> (defaultBody | body) ~ (body*) ^^ { case first ~ rest => first :: rest }
    
    declaration ~ (noBody | bodies) ^^ { case (name, attrs) ~ bodies => ParsedDirective(name, attrs ::: bodies) }
  }
  
  abstract class DirectiveContextBase (parts: PartMap, docContext: Option[DocumentContext] = None) {
    
    def part (key: Key): Option[String] = parts.get(key)
      
    val context: DocumentContext = docContext.get // TODO - change API - needs to be Option when executed in 1st phase
    
  }
  
  lazy val spanDirective: Parser[Span] = {
    translate { directiveParser(null) ^^ { result => // TODO - specify span parser - must keep parsed spans cached in addition to source string, must deal with ws
      def createContext (parts: PartMap, docContext: Option[DocumentContext]):Spans.DirectiveContext = {
        new DirectiveContextBase(parts, docContext) with Spans.DirectiveContext {
          val parser = new Spans.Parser {
            def apply (source: String) = parseInline(source)
          }
        }
      }
      applyDirective(Spans)(result, spanDirective, createContext)
    }}
  }
  
  
  def applyDirective [E <: Element](builder: BuilderContext[E])
      (parseResult: ParsedDirective, 
       directives: String => Option[builder.Directive], 
       contextFactory: (PartMap, Option[DocumentContext]) => builder.DirectiveContext): Directives.Result[E] = {
    
    import laika.util.Builders.{~ => ~~}
    
    val directive = directives(parseResult.name).map(Directives.Success(_))
        .getOrElse(Directives.Failure("No span directive registered with name: "+parseResult.name))
    
    val context = {
      val dups = parseResult.parts groupBy (_.key) filterNot (_._2.tail.isEmpty) keySet;
      if (dups.isEmpty) Directives.Success(contextFactory(parseResult.parts map (p => (p.key, p.content)) toMap, None))
      else Directives.Failure(dups.map("Duplicate "+_.desc).toList)
    }
    
    (directive ~ context) flatMap { case directive ~~ context =>
      directive(context)
    }
    
    // TODO - requirements to check whether Placeholder creation is necessary
  }
  
  def translate [E] (parser: Parser[Directives.Result[E]]): Parser[E] = Parser { in =>
      
    parser(in) match {
      case Success(result, next) => result match {
        case Directives.Success(result) => Success(result, next)
        case Directives.Failure(messages) => Failure("One or more errors processing directive: " + messages.mkString(", "), in)
      } 
      case ns: NoSuccess => ns
    }
      
  }
  
  
  private case class InvalidDirective (msg: String, source: String, options: Options = NoOpt) extends Block with Span
  
  private def directive [E](p: Parser[E], name: String) = Parser { in =>
    p(in) match {
      case s @ Success(_,_) => s
      case NoSuccess(msg, next) => (indentedBlock() ^^ { block =>
        InvalidDirective(msg, ".. "+name+" "+(block.lines mkString "\n")).asInstanceOf[E] // TODO - for these directive types we want a warning and then continue parsing with other parsers
      })(in)
    }
  }
  
  private def replaceInvalidDirective (block: Block) = block match {
    case InvalidDirective(msg, source, _) => InvalidBlock(SystemMessage(laika.tree.Elements.Error, msg), LiteralBlock(source))
    case other => other
  }
  

  
}