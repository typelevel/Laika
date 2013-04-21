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

/** Provides the parsers for all types of explicit block elements.
 *  In reStructuredText an explicit block element starts with `.. `,
 *  followed by a block where the second and subsequent lines are indented.
 * 
 * @author Jens Halm
 */
trait ExplicitBlockParsers extends BlockBaseParsers { self: InlineParsers =>

  
  
  private val explicitStart = (".." ~ (ws min 1))
  
  
  /** Parses all types of explicit block items.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#explicit-markup-blocks]].
   */
  def explicitBlockItem: Parser[Block] = (explicitStart ~>
    (footnote | citation | linkTarget | substitutionDefinition | roleDirective | blockDirective | comment)) |
    shortAnonymousLinkTarget
  

  /** Parses a footnote.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnotes]]. 
   */
  def footnote = {
    val prefix = '[' ~> footnoteLabel <~ ']' ~ ws
    
    prefix ~ varIndentedBlock() ^^ {
      case label ~ block => FootnoteDefinition(label, parseNestedBlocks(block))
    }
  }
  
  /** Parses a citation.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citations]]. 
   */
  def citation = {
    val prefix = '[' ~> simpleRefName <~ ']' ~ ws
    
    prefix ~ varIndentedBlock() ^^ {
      case label ~ block => Citation(label, parseNestedBlocks(block))
    }
  }
  
  /** Parses the short variant of an anonymous link definition
   *  (that starts with `__` instead of `.. __:`)
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#anonymous-hyperlinks]].
   */
  lazy val shortAnonymousLinkTarget = {
    "__ " ~> linkDefinitionBody ^^ { body => ExternalLinkDefinition("", body) } 
  }
  
  private lazy val linkDefinitionBody = {
    val notEmpty = not(blankLine) | guard(restOfLine ~ (ws min 1) ~ not(blankLine))
    
    (notEmpty ~> varIndentedBlock()) ^^ { 
      _.lines map (_.trim) filterNot (_.isEmpty) mkString
    }
  }
  
  /** Parses a link definition, either an internal, external or indirect link.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-targets]].
   */
  def linkTarget = {
    
    val named = '_' ~> (refName) <~ ':' ^^ { _.normalized }
      
    val internal = named ^^ (InternalLinkTarget(_)) // TODO - might need special logic for cases where it points to other targets (e.g. external)
    
    val external = {
      val anonymous = "__:" ^^^ ""
    
      (named | anonymous) ~ linkDefinitionBody ^^ {
        case name ~ body => ExternalLinkDefinition(name, body)
      }
    }
    
    val indirect = {
      (named <~ ws) ~ ((opt(eol ~ ws) ~ "`" ~> escapedText(anyUntil('`')) | simpleRefName) <~ '_' ~ ws ~ eol) ^^ {
        case name ~ refName => IndirectLinkDefinition(name, LinkReference(Nil, refName.replaceAll("\n", ""), "`" + refName + "`_")) 
        // TODO - source value might be without back ticks
      }
    }
    
    indirect | external | internal
  }
  
  /** Parses a substitution definition.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-definitions]].
   */
  def substitutionDefinition = {
    val text = not(ws take 1) ~> escapedText(anyBut('|','\n') min 1)  
    val prefix = '|' ~> text <~ not(lookBehind(1, ' ')) ~ '|'
    
    ((prefix <~ ws) ~ spanDirective) ^^ { 
      case name ~ InvalidDirective(msg, source, _) => 
        InvalidBlock(SystemMessage(laika.tree.Elements.Error, msg), LiteralBlock(source.replaceFirst(".. ",".. |"+name+"| ")))
      case name ~ content => SubstitutionDefinition(name, content) 
    }
  }
  
  /** Parses a comment.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#comments]].
   */
  def comment = {
    varIndentedBlock() ^^ { block =>
      Comment((block.lines map (_.trim) mkString "\n").trim)
    }
  }
  
  
  /** Mapping from the name of all configured block directives to their implementation.
   * 
   *  See [[laika.parse.rst.Directives]] for details on how to implement directives.  
   */
  def blockDirectives: Map[String, DirectivePart[Block]]
  
  /** Mapping from the name of all configured span directives to their implementation.
   * 
   *  See [[laika.parse.rst.Directives]] for details on how to implement directives.  
   */
  def spanDirectives: Map[String, DirectivePart[Span]]
  
  /** Mapping from the name of all configured text roles to their implementation.
   * 
   *  See [[laika.parse.rst.TextRoles]] for details on how to implement text roles.  
   */
  def textRoles: Map[String, TextRole]
    

  private def replaceInvalidDirective (block: Block) = block match {
    case InvalidDirective(msg, source, _) => InvalidBlock(SystemMessage(laika.tree.Elements.Error, msg), LiteralBlock(source))
    case other => other
  }
  
  /** Parses a block-level directive.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]].
   */
  def blockDirective: Parser[Block] = directive(blockDirectives) ^^ replaceInvalidDirective

  private def spanDirective = directive(spanDirectives)
  
  private def directive [E](directives: Map[String, DirectivePart[E]]): Parser[E] = {
    
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
  
  private case class InvalidDirective (msg: String, source: String, options: Options = NoOpt) extends Block with Span
  
  private def directive [E](p: Parser[E], name: String) = Parser { in =>
    p(in) match {
      case s @ Success(_,_) => s
      case NoSuccess(msg, next) => (varIndentedBlock() ^^ { block =>
        InvalidDirective(msg, ".. "+name+" "+(block.lines mkString "\n")).asInstanceOf[E]
      })(in)
    }
  }

  /** Parses a role directive.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles]].
   */
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
      directive(textRoles.get(base).map(directiveParser(name))
          .getOrElse(failure("unknown text role: " + base)), fullname) ^^ replaceInvalidDirective
    }
    
  }
  
  /** Overrides the failure message to the specified parser.
   */
  def withFailureMessage [T](p: => Parser[T], msg: String) = Parser { in =>
    // TODO - obsolete when moving to 2.10
      p(in) match {
        case Failure(_, next) => Failure(msg, next)
        case other            => other
      }
    }

  
  private class DirectiveParserBuilder extends DirectiveParser {

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
    
    val body = lookBehind(1, '\n') ~> varIndentedBlock(firstLineIndented = true) | varIndentedBlock()
    
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
    
    def blockContent: Result[Seq[Block]] = parseContentWith {
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
  
  private class RoleDirectiveParserBuilder (delegate: DirectiveParser) extends RoleDirectiveParser {
    
    def field [T](name: String, f: String => Either[String,T]): Result[T] = delegate.field(name,f)
    
    def optField [T](name: String, f: String => Either[String,T]): Result[Option[T]] = delegate.optField(name,f)
    
    def blockContent: Result[Seq[Block]] = delegate.blockContent
    
    def content [T](f: String => Either[String,T]): Result[T] = delegate.content(f)
    
  }
    
  
}