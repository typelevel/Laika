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
import laika.tree.Templates.PlaceholderSpan
import laika.tree.Templates.PlaceholderBlock
import laika.template.TemplateParsers
import laika.tree.Templates.TemplateElement

/** 
 * @author Jens Halm
 */
trait DirectiveParsers extends laika.parse.InlineParsers { 
  
  
  def withSource[T] (p: Parser[T]): Parser[(T, String)] = Parser { in =>
    p(in) match {
      case Success(result, next) => Success((result, next.source.subSequence(in.offset, next.offset).toString), next)
      case ns: NoSuccess         => ns
    }
  }
  
  
  def escapedUntil (char: Char) = escapedText(anyUntil(char) min 1)

  def escapedText (p: TextParser) = text(p, Map('\\' -> (any take 1))) // TODO - should be promoted to generic inline parser (after which this trait only needs to extend MarkupParsers)
  
  
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
      
    val context = docContext
    
  }
  
  def applyDirective [E <: Element](builder: BuilderContext[E])
      (parseResult: ParsedDirective, 
       directives: String => Option[builder.Directive], 
       createContext: (PartMap, Option[DocumentContext]) => builder.DirectiveContext,
       createPlaceholder: (DocumentContext => E) => E, 
       createInvalidElement: String => E): E = {
    
    import laika.util.Builders.{~ => ~~}
    
    val directive = directives(parseResult.name).map(Directives.Success(_))
        .getOrElse(Directives.Failure("No span directive registered with name: "+parseResult.name))
    
    val partMap = {
      val dups = parseResult.parts groupBy (_.key) filterNot (_._2.tail.isEmpty) keySet;
      if (dups.isEmpty) Directives.Success(parseResult.parts map (p => (p.key, p.content)) toMap)
      else Directives.Failure(dups.map("Duplicate "+_.desc).toList)
    }
    
    def processResult (result: Directives.Result[E]) = result match {
      case Directives.Success(result)   => result
      case Directives.Failure(messages) => createInvalidElement("One or more errors processing directive: " + messages.mkString(", "))
    }
    
    processResult((directive ~ partMap) flatMap { case directive ~~ partMap =>
      def directiveWithContext (context: Option[DocumentContext]) = directive(createContext(partMap, context))
      if (directive.requiresContext) Directives.Success(createPlaceholder(c => processResult(directiveWithContext(Some(c)))))
      else directiveWithContext(None)
    }) 
  }
  
}

object DirectiveParsers {
  
  
  trait TemplateDirectives extends DirectiveParsers with TemplateParsers {
    
    def getTemplateDirective (name: String): Option[Templates.Directive]
    
    // TODO - verify correctness of TemplateElement usage
    
    case class DirectiveSpan (f: DocumentContext => Span, options: Options = NoOpt) extends PlaceholderSpan {
      def resolve (context: DocumentContext) = TemplateElement(f(context))
    }
    
    lazy val spanDirective: Parser[Span] = {
      val bodyContent = withSource(wsOrNl ~ '{' ~> spans(anyUntil('}'), spanParsers) <~ wsOrNl) ^^ (_._2)
      directiveParser(bodyContent) ^^ { result => // TODO - optimization - parsed spans might be cached for DirectiveContext
        
        def createContext (parts: PartMap, docContext: Option[DocumentContext]): Templates.DirectiveContext = {
          new DirectiveContextBase(parts, docContext) with Templates.DirectiveContext {
            val parser = new Templates.Parser {
              def apply (source: String) = List(TemplateElement(SpanSequence(parseInline(source)))) // TODO - delegate to new template parser
            }
          }
        }
        def invalid (msg: String) = TemplateElement(InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Literal(""))) // TODO - error handling - pass source
        
        applyDirective(Templates)(result, getTemplateDirective, createContext, s => TemplateElement(DirectiveSpan(s)), invalid)
      }
    }
    
  }
  
  
  trait SpanDirectives extends DirectiveParsers {
    
    def getSpanDirective (name: String): Option[Spans.Directive]
    
    case class DirectiveSpan (f: DocumentContext => Span, options: Options = NoOpt) extends PlaceholderSpan {
      def resolve (context: DocumentContext) = f(context)
    }
    
    lazy val spanDirective: Parser[Span] = {
      val bodyContent = withSource(wsOrNl ~ '{' ~> spans(anyUntil('}'), spanParsers) <~ wsOrNl) ^^ (_._2)
      directiveParser(bodyContent) ^^ { result => // TODO - optimization - parsed spans might be cached for DirectiveContext
        
        def createContext (parts: PartMap, docContext: Option[DocumentContext]): Spans.DirectiveContext = {
          new DirectiveContextBase(parts, docContext) with Spans.DirectiveContext {
            val parser = new Spans.Parser {
              def apply (source: String) = parseInline(source)
            }
          }
        }
        def invalid (msg: String) = InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Literal("")) // TODO - error handling - pass source
        
        applyDirective(Spans)(result, getSpanDirective, createContext, DirectiveSpan(_), invalid)
      }
    }
    
  }
  
  
  trait BlockDirectives extends DirectiveParsers with laika.parse.BlockParsers {
    
    def getBlockDirective (name: String): Option[Blocks.Directive]
    
    case class DirectiveBlock (f: DocumentContext => Block, options: Options = NoOpt) extends PlaceholderBlock {
      def resolve (context: DocumentContext) = f(context)
    }
    
    lazy val spanDirective: Parser[Block] = {
      val bodyContent = withSource(wsOrNl ~ '{' ~> spans(anyUntil('}'), spanParsers) <~ wsOrNl) ^^ (_._2) // TODO - needs to be indented block
      directiveParser(bodyContent) ^^ { result => // TODO - optimization - parsed spans might be cached for DirectiveContext
        
        def createContext (parts: PartMap, docContext: Option[DocumentContext]): Blocks.DirectiveContext = {
          new DirectiveContextBase(parts, docContext) with Blocks.DirectiveContext {
            val parser = new Blocks.Parser {
              def apply (source: String): Seq[Block] = parseNestedBlocks(source.split("\n").toList, 0) // TODO - avoid split
              def parseInline (source: String): Seq[Span] = parseInline(source)
            }
          }
        }
        def invalid (msg: String) = InvalidBlock(SystemMessage(laika.tree.Elements.Error, msg), LiteralBlock("")) // TODO - error handling - pass source
        
        applyDirective(Blocks)(result, getBlockDirective, createContext, DirectiveBlock(_), invalid)
      }
    }
    
  }
  
  
}