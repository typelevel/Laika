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
import laika.parse.rst.Elements._
import laika.util.Builders._
import laika.parse.rst.Elements.SubstitutionDefinition
import Directives._
import TextRoles._
import scala.collection.mutable.ListBuffer

/**
 * @author Jens Halm
 */
trait ExplicitBlockParsers extends BlockBaseParsers { self: InlineParsers =>

  
  
  val explicitStart = (".." ~ (ws min 1))
  
  
  def explicitBlockItem: Parser[Block] = (explicitStart ~>
    (footnote | citation | linkDefinition | substitutionDefinition | roleDirective | blockDirective | comment)) |
    shortAnonymousLinkDefinition
  
  
  def footnote = {
    val prefix = '[' ~> footnoteLabel <~ ']' ~ ws
    
    prefix ~ varIndentedBlock() ^^ {
      case label ~ block => Footnote(label, parseNestedBlocks(block))
    }
  }
  
  def citation = {
    val prefix = '[' ~> simpleRefName <~ ']' ~ ws
    
    prefix ~ varIndentedBlock() ^^ {
      case label ~ block => Citation(label, parseNestedBlocks(block))
    }
  }
  
  lazy val shortAnonymousLinkDefinition = {
    "__ " ~> linkDefinitionBody ^^ { body => LinkDefinition("", body) } 
  }
  
  private lazy val linkDefinitionBody = {
    val notEmpty = not(blankLine) | guard(restOfLine ~ (ws min 1) ~ not(blankLine))
    
    (notEmpty ~> varIndentedBlock()) ^^ { 
      _.lines map (_.trim) filterNot (_.isEmpty) mkString
    }
  }
  
  def linkDefinition = {
    
    val named = '_' ~> (refName) <~ ':' ^^ { _.normalized }
      
    val internal = named ^^ InternalLinkTarget // TODO - might need special logic for cases where it points to other targets (e.g. external)
    
    val external = {
      val anonymous = "__:" ^^^ ""
    
      (named | anonymous) ~ linkDefinitionBody ^^ {
        case name ~ body => LinkDefinition(name, body)
      }
    }
    
    val indirect = {
      (named <~ ws) ~ ((opt(eol ~ ws) ~ "`" ~> escapedText(anyUntil('`')) | simpleRefName) <~ '_' ~ ws ~ eol) ^^ {
        case name ~ refName => IndirectLinkTarget(name, LinkReference(Nil, refName.replaceAll("\n", ""), "`", "`_")) 
      }
    }
    
    indirect | external | internal
  }
  
  
  def substitutionDefinition = {
    val text = not(ws take 1) ~> escapedText(anyBut('|','\n') min 1)  
    val prefix = '|' ~> text <~ not(lookBehind(1, ' ')) ~ '|'
    
    ((prefix <~ ws) ~ spanDirective) ^^ { 
      case name ~ InvalidDirective(msg, CodeBlock(content)) => 
        SubstitutionDefinition(name, InvalidDirective(msg, CodeBlock(content.replaceFirst(".. ",".. |"+name+"| ")))) 
      case name ~ content => SubstitutionDefinition(name, content) 
    }
  }
  
  def comment = {
    varIndentedBlock() ^^ { block =>
      Comment((block.lines map (_.trim) mkString "\n").trim)
    }
  }
  
  
  def blockDirectives: Map[String, DirectivePart[Block]]
  
  def spanDirectives: Map[String, DirectivePart[Span]]
  
  def textRoles: Map[String, TextRole]
    

  def blockDirective: Parser[Block] = directive(blockDirectives)

  def spanDirective = directive(spanDirectives)
  
  protected def directive [E](directives: Map[String, DirectivePart[E]]): Parser[E] = {
    
    val nameParser = simpleRefName <~ "::" ~ ws
    
    def directiveParser [E] (directive: DirectivePart[E]) = {
      val parserBuilder = new DirectiveParserBuilder
      val result = directive(parserBuilder)
      parserBuilder.parser ^^^ {
        result.get
      }
    }
    
    nameParser >> { name => 
      directive(directives.get(name).map(directiveParser).getOrElse(failure("unknown directive: " + name)), name+"::")
    }
  }
  
  private def directive [E](p: Parser[E], name: String) = Parser { in =>
    p(in) match {
      case s @ Success(_,_) => s
      case NoSuccess(msg, next) => (varIndentedBlock() ^^ { block =>
        InvalidDirective(SystemMessage(laika.tree.Elements.Error, msg), 
              CodeBlock(".. "+name+" "+(block.lines mkString "\n"))).asInstanceOf[E]
      })(in)
    }
  }

  def roleDirective = {
    
    val nameParser = "role::" ~ ws ~> simpleRefName ~ opt('(' ~> simpleRefName <~ ')')
    
    def directiveParser (name: String)(role: TextRole): Parser[Block] = {
      val delegate = new DirectiveParserBuilder
      val parserBuilder = new RoleDirectiveParserBuilder(delegate)
      val result = role.part(parserBuilder)
      delegate.parser ^^^ {
        CustomizedTextRole(name, result.get)
      }
    }
    
    nameParser >> { case name ~ baseName =>
      val base = baseName.getOrElse(defaultTextRole)
      val fullname = "role::" + name + (baseName map ("("+_+")") getOrElse "")
      directive(textRoles.get(base).map(directiveParser(name)).getOrElse(failure("unknown text role: " + base)), fullname) ^^ { 
        case inv: InvalidDirective => InvalidTextRole(name, inv)
        case other => other
      }
    }
    
  }
  
  def withFailureMessage [T](p: => Parser[T], msg: String) = Parser { in =>
    // TODO - obsolete when moving to 2.10
      p(in) match {
        case Failure(_, next) => Failure(msg, next)
        case other            => other
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
    
    def requiredArg (p: => Parser[String]) = withFailureMessage(p, "missing required argument")                          
                              
    val arg = requiredArg((anyBut(' ','\n') min 1) <~ ws)
    
    val argWithWS = {
      val argLineStart = not(blankLine | ':')
      val nextBlock = failure("args do not expand beyond blank lines")
      val p = varIndentedBlock(1, argLineStart, nextBlock) ^^? { block =>
        val text = (block.lines mkString "\n").trim
        if (text.nonEmpty) Right(text) else Left("missing required argument")
      }
      requiredArg(p)
    }
    
    val body = lookBehind(1, '\n') ~> varIndentedBlock(testFirstLine = true) | varIndentedBlock()
    
    // TODO - some duplicate logic with original fieldList parser
    lazy val directiveFieldList: Parser[Any] = {
      
      val name = ':' ~> escapedUntil(':')
      
      val firstLine = anyBut('\n') 
      
      val item = (ws min 1) >> { firstIndent =>
          (name ~ firstLine ~ opt(varIndentedBlock(firstIndent.length + 1, not(":"), failure("blank lines terminate options")))) ^^ 
        { case name ~ firstLine ~ Some(block) => 
            (name, ((firstLine :: block.lines.tail) mkString "\n").trim)
          case name ~ firstLine ~ None => 
            (name, firstLine.trim) }}
      
      opt(ws ~ eol) ~> (item *) ^^? { fields =>
        
        val parsed = scala.collection.mutable.Map(fields.toArray:_*)
        val missing = scala.collection.mutable.Set.empty[String]
        val invalid = new ListBuffer[String]
        
        for ((name, f) <- requiredFields) {
          parsed.remove(name).map(res => f(res).left map {invalid += name + ": " + _}).getOrElse(missing += name)
        }
        for ((name, f) <- optionalFields) {
          parsed.remove(name).map(res => f(res).left map {invalid += name + ": " + _})
        }
        
        val errors = new ListBuffer[String]
        if (parsed.nonEmpty) errors += parsed.map(_._1).mkString("unknown options: ",", ","")
        if (missing.nonEmpty) errors += missing.mkString("missing required options: ",", ","")
        if (invalid.nonEmpty) errors += invalid.mkString("invalid option values: ", ", ", "") 
        Either.cond(errors.isEmpty, (), errors mkString "; ")
      }
    }
    
    val contentSeparator = (lookBehind(1, '\n') | eol) ~ blankLine
    
    val requiredFields: scala.collection.mutable.Map[String, String => Either[String,Unit]] = scala.collection.mutable.Map()
    val optionalFields: scala.collection.mutable.Map[String, String => Either[String,Unit]] = scala.collection.mutable.Map()
    
    class LazyResult[T] {
      var value: Option[T] = None
      def set (v: T) = value = Some(v)
      def set (v: Option[T]) = value = v
      def get: T = value getOrElse { throw new IllegalStateException("result not set yet") }
    }
    
    def optionToEither [T](f: String => Either[String,T])(res: Option[String]): Either[String,Option[T]] =
      (res map f) map (_.right map (res => Some(res))) getOrElse Right(None)
    
    def argument [T](convert: String => Either[String,T] = {s:String => Right(s)}, 
                     withWS: Boolean = false) = {
      separator = contentSeparator
      val result = new LazyResult[T]
      if (withWS) requiredArgWithWS = (argWithWS ^^? convert ^^ result.set)
      else requiredArgs = requiredArgs ~ (arg ^^? convert ^^ result.set)
      new Result(result.get)
    }
    
    def optArgument [T](convert: String => Either[String,T] = {s:String => Right(s)}, 
                        withWS: Boolean = false) = {
      separator = contentSeparator
      val result = new LazyResult[T]
      if (withWS) optionalArgWithWS = (opt(argWithWS) ^^? optionToEither(convert) ^^ result.set)
      else optionalArgs = optionalArgs ~ (opt(arg) ^^? optionToEither(convert) ^^ result.set)
      new Result(result.value)
    }
    
    def field [T](name: String, f: String => Either[String,T]): Result[T] = {
      separator = contentSeparator
      fields = directiveFieldList
      val result = new LazyResult[T]
      requiredFields += (name -> { s:String => f(s).right map result.set })
      new Result(result.get)
    }
    
    def optField [T](name: String, f: String => Either[String,T]): Result[Option[T]] = {
      separator = contentSeparator
      fields = directiveFieldList
      val result = new LazyResult[T]
      optionalFields += (name -> { s:String => f(s).right map result.set })
      new Result(result.value)
    }
    
    def standardContent: Result[Seq[Block]] = parseContentWith {
      block => Right(parseNestedBlocks(block))
    }
    
    def content [T](f: String => Either[String,T]): Result[T] = parseContentWith {
      block => f(block.lines mkString "\n")
    }
    
    private def parseContentWith [T](f: IndentedBlock => Either[String,T]): Result[T] = {
      val result = new LazyResult[T]
      contentParser = (body ^^? f ^^ result.set)
      new Result(result.get)
    }
    
  }
  
  class RoleDirectiveParserBuilder (delegate: DirectiveParser) extends RoleDirectiveParser {
    
    def field [T](name: String, f: String => Either[String,T]): Result[T] = delegate.field(name,f)
    
    def optField [T](name: String, f: String => Either[String,T]): Result[Option[T]] = delegate.optField(name,f)
    
    def standardContent: Result[Seq[Block]] = delegate.standardContent
    
    def content [T](f: String => Either[String,T]): Result[T] = delegate.content(f)
    
  }
    
  
}