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

import laika.parse.InlineParsers
import laika.tree.Elements._
import laika.util.Builders._
import laika.parse.rst.Elements.SubstitutionDefinition
import Directives._
import scala.collection.mutable.ListBuffer

/**
 * @author Jens Halm
 */
trait ExplicitBlockParsers extends BlockBaseParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers 

  
  // TODO - might be needed in some base trait
  val simpleRefName = {
    val alphanum = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1 // TODO - check whether non-ascii characters are allowed
    val symbol = anyOf('-', '_', '.', ':', '+') take 1
    
    alphanum ~ ((symbol ~ alphanum)*) ^^ { 
      case start ~ rest => start + (rest map { case a~b => a+b }).mkString
    } // TODO - normalize ws - lowercase
  }
  
  val refName = simpleRefName | phraseRef
  
  val phraseRef = '`' ~> (anyBut('`') min 1) <~ '`'
  
  
  /** TODO - move to base trait - duplicated from ListParsers
   *  nestedBlock parser in BlockParsers not used very often
   */
  def nestedBlocks = 
    if (true) (standardRstBlock | paragraph) * // TODO - needs nest level check
    else (nonRecursiveRstBlock | paragraph) *
  
  
  def explicitStart = (".." ~ (ws min 1)) ^^ { case _ ~ ws => ws.length + 2 }
  
  
  def explicitBlockItems = explicitStart >> { len => // TODO - length not needed when tab processing gets changed
    footnote(len) | citation(len) | linkDefinition(len) | substitutionDefinition(len) // TODO - there is a linkDef alternative not requiring the .. prefix
  }
  
  
  def footnote (prefixLength: Int) = {
    val decimal = (anyIn('0' to '9') min 1) ^^ { n => NumericLabel(n.toInt) }
    val autonumber = '#' ^^^ Autonumber 
    val autosymbol = '*' ^^^ Autosymbol
    val autonumberLabel = '#' ~> simpleRefName ^^ AutonumberLabel 
    
    val label = decimal | autonumberLabel | autonumber | autosymbol
    
    val prefix = '[' ~> label <~ ']'
    
    def labelLength (label: FootnoteLabel) = { // TODO - not needed when tab processing gets changed
      val len = label match {
        case Autonumber | Autosymbol => 3
        case AutonumberLabel(label) => label.length + 2
        case NumericLabel(number: Int) => number.toString.length + 2
      }
      len + prefixLength
    }
    
    guard(prefix) >> { label => // TODO - parsing prefix twice is inefficient, indentedBlock parser should return result
      indentedBlock(prefix ^^ labelLength) ^^ {
        case (lines,pos) => Footnote(label, parseMarkup(nestedBlocks, lines mkString "\n"))
      }
    }
  }
  
  def citation (prefixLength: Int) = {
    val prefix = '[' ~> simpleRefName <~ ']'
    
    guard(prefix) >> { label => // TODO - parsing prefix twice is inefficient, indentedBlock parser should return result
      indentedBlock(prefix ^^ { _.length + prefixLength }) ^^ {
        case (lines,pos) => Citation(label, parseMarkup(nestedBlocks, lines mkString "\n"))
      }
    }
  }
  
  def linkDefinition (prefixLength: Int) = {
    val named = '_' ~> refName <~ ':' // TODO - backticks are optional here in most cases (unless ref name contains colons)
    val anonymous = "__:"
      
    val internal = named ^^ InternalLinkTarget // TODO - might need special logic for cases where it points to other targets (e.g. external)
    
    val externalName = (named | anonymous)
    
    val external = guard(externalName) >> { name =>
      indentedBlock(externalName ^^ { _.length + prefixLength }) ^^ { // TODO - indentedBlock parser still requires ws after prefix
        case (lines,pos) => LinkDefinition(name, lines map (_.trim) filter (_.isEmpty) mkString)
      }
    }
    
    external | internal
    
    // TODO - add indirect targets once inline parsers are completed
  }
  
  def substitutionDefinition (prefixLength: Int) = {
    val text = not(ws take 1) ~> (anyBut('|','\n') min 1)  
    val prefix = '|' ~> text <~ not(lookBehind(1, ' ')) ~ '|'
    
    ((prefix <~ ws) ~ spanDirective) ^^ { case name ~ content => SubstitutionDefinition(name, content) }
  }
  
  def comment (prefixLength: Int) = {
    indentedBlock(success(prefixLength)) ^^ { // TODO - indentedBlock parser still requires ws after prefix
      case (lines,pos) => Comment(lines map (_.trim) filter (_.isEmpty) mkString)
    }
  }
  
  
  val blockDirectives: Map[String, DirectivePart[Seq[Block]]] // TODO - populate (maybe do this in reStructuredText entry point object)
  
  val spanDirectives: Map[String, DirectivePart[Seq[Span]]]
    

  def blockDirective = directive(blockDirectives)

  def spanDirective = directive(spanDirectives)
  
  def directive [E](directives: Map[String, DirectivePart[Seq[E]]]) = {
    
    val nameParser = simpleRefName <~ "::" ~ ws
    
    def directiveParser [E] (directive: DirectivePart[Seq[E]]) = {
      val parserBuilder = new DirectiveParserBuilder
      val result = directive(parserBuilder)
      parserBuilder.parser ^^^ {
        result.get
      }
    }
    
    nameParser >> { name =>
      directives.get(name).map(directiveParser).getOrElse(failure("unknown directive: " + name))
    }
    
  }
  
  // TODO - deal with failures and exact behaviour for unknown directives and other types of error
  class DirectiveParserBuilder extends DirectiveParser {

    val skip = success(())
    var requiredArgs: Parser[Any] = skip
    var optionalArgs: Parser[Any] = skip
    var requiredArgWithWS: Parser[Any] = skip
    var optionalArgWithWS: Parser[Any] = skip
    var fields: Parser[Any] = skip
    var separator: Parser[Any] = skip
    var contentParser: Parser[Any] = skip
    
    def parser: Parser[Any] = requiredArgs ~ requiredArgWithWS ~ 
                              optionalArgs ~ optionalArgWithWS ~
                              fields ~ separator ~ contentParser
    
    val arg = (anyBut(' ','\n') min 1) <~ ws
    
    val argWithWS = {
      val argLine = not(blankLine | ':')
      val nextBlock = failure("args do not expand beyond blank lines")
      lineAndIndentedBlock(argLine ^^^ 0, argLine, nextBlock) ^^ { case (lines,pos) =>
        lines mkString "\n"
      }
    }
    
    val body = indentedBlock(success(1)) ^^ { case (lines, pos) => lines mkString "\n" }
    
    // TODO - some duplicate logic with original fieldList parser
    lazy val directiveFieldList: Parser[Any] = {
      
      val name = eol ~ ':' ~> anyBut(':') <~ ':' // TODO - must be escapedUntil(':') once InlineParsers are implemented
      
      val firstLine = restOfLine 
      
      val item = (ws min 1) >> { firstIndent =>
          (name ~ firstLine ~ opt(indentedBlock(success(firstIndent.length)))) ^^ 
        { case name ~ firstLine ~ Some((lines, pos)) => 
            (name, (firstLine :: lines) mkString "\n")
          case name ~ firstLine ~ None => 
            (name, firstLine) }}
      
      (item *) ^^? { fields =>
        val parsed = scala.collection.mutable.Map(fields.toArray:_*)
        val missing = scala.collection.mutable.Set.empty[String]
        
        for ((name, f) <- requiredFields) {
          parsed.remove(name).map(f).getOrElse(missing += name)
        }
        for ((name, f) <- optionalFields) {
          parsed.remove(name).foreach(f)
        }
        
        val errors = new ListBuffer[String]
        if (parsed.nonEmpty) errors += parsed.mkString("unknown options: ",", ","")
        if (missing.nonEmpty) errors += parsed.mkString("missing required options: ",", ","")
        Either.cond(errors.isEmpty, (), errors mkString "; ")
      }
    }
    
    val contentSeparator = (lookBehind(1, '\n') | eol) ~ blankLine
    
    val requiredFields: Map[String, String => Unit] = Map.empty
    val optionalFields: Map[String, String => Unit] = Map.empty
    
    class LazyResult[T] {
      var value: Option[T] = None
      def set (v: T) = value = Some(v)
      def get: T = value getOrElse { throw new IllegalStateException("result not set yet") }
      def fromString (f: String => T)(s: String) = set(f(s))
    }
    
    def requiredArg [T](f: String => T): Result[T] = {
      separator = contentSeparator
      val result = new LazyResult[T]
      requiredArgs = requiredArgs ~ (arg ^^ result.fromString(f))
      new Result(result.get)
    }
    
    def requiredArgWithWS [T](f: String => T): Result[T] = {
      separator = contentSeparator
      val result = new LazyResult[T]
      requiredArgWithWS = (argWithWS ^^ result.fromString(f))
      new Result(result.get)
    }
    
    def optionalArg [T](f: String => T): Result[Option[T]] = {
      separator = contentSeparator
      val result = new LazyResult[T]
      optionalArgs = optionalArgs ~ (arg ^^ result.fromString(f))
      new Result(result.value)
    }
    
    def optionalArgWithWS [T](f: String => T): Result[T] = {
      separator = contentSeparator
      val result = new LazyResult[T]
      optionalArgWithWS = (argWithWS ^^ result.fromString(f))
      new Result(result.get)
    }
    
    def requiredField [T](name: String, f: String => T): Result[T] = {
      separator = contentSeparator
      fields = directiveFieldList
      val result = new LazyResult[T]
      requiredFields + (name -> result.fromString(f)_)
      new Result(result.get)
    }
    
    def optionalField [T](name: String, f: String => T): Result[Option[T]] = {
      separator = contentSeparator
      fields = directiveFieldList
      val result = new LazyResult[T]
      optionalFields + (name -> result.fromString(f)_)
      new Result(result.value)
    }
    
    def standardContent: Result[Seq[Block]] = content { rawtext =>
      parseMarkup(nestedBlocks, rawtext)
    }
    
    def content [T](f: String => T): Result[T] = {
      val result = new LazyResult[T]
      contentParser = (body ^^ result.fromString(f))
      new Result(result.get)
    }
    
  }
    
  
}