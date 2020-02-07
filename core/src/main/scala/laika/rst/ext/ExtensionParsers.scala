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

package laika.rst.ext

import laika.ast._
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.parse.markup.RecursiveParsers
import laika.parse.text.{DelimitedText, PrefixedParser}
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.Parser
import laika.rst.BaseParsers._
import laika.rst.ast.{CustomizedTextRole, SubstitutionDefinition}
import laika.rst.bundle.RstExtension
import laika.rst.ext.Directives._
import laika.rst.ext.ExtensionParsers.Result
import laika.rst.ext.TextRoles._

/** Provides the parsers for all types of extensions (directives and text roles).
 *
 * @author Jens Halm
 */
class ExtensionParsers(recParsers: RecursiveParsers,
                       blockDirectives: Map[String, DirectivePartBuilder[Block]],
                       spanDirectives: Map[String, DirectivePartBuilder[Span]],
                       textRoles: Map[String, RoleDirectivePartBuilder[String => Span]],
                       defaultTextRole: String) {


  import recParsers._

  
  private val explicitStart = ".." ~ ws.min(1)
  
  
  /** Parses all types of explicit block items.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#explicit-markup-blocks]].
   */
  lazy val explicitBlockItem: PrefixedParser[Block] = 
    explicitStart ~> (substitutionDefinition | roleDirective | blockDirective)
  
  /** Parses a substitution definition.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-definitions]].
   */
  lazy val substitutionDefinition: Parser[Block] = {
    val prefix = delimiter('|').nextNot(' ') ~> 
      escapedText(delimitedBy(delimiter('|').prevNot(' ')).failOn('\n').nonEmpty)
    
    ((prefix <~ ws) ~ spanDirectiveParser).map { 
      case name ~ InvalidDirective(msg, source, _) =>
        InvalidElement(msg, source.replaceFirst(".. ",s".. |$name| ")).asBlock
      case name ~ content => SubstitutionDefinition(name, content) 
    }
  }
  private lazy val spanDirectiveParser: Parser[Span] = directive(spanDirectives.get)
  
  private def replaceInvalidDirective (block: Block): Block = block match {
    case InvalidDirective(msg, source, _) => InvalidElement(msg, source).asBlock
    case other => other
  }
  
  /** Parses a block-level directive.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]].
   */
  lazy val blockDirective: Parser[Block] = directive(blockDirectives.get).map(replaceInvalidDirective)

  private val recBlocks: String => Result[Seq[Block]] = 
    s => recursiveBlocks(DelimitedText.Undelimited).parse(s).toEither
  
  private val recSpans: String => Result[Seq[Span]] = s => recursiveSpans.parse(s).toEither
  
  private def directive [E](provider: String => Option[DirectivePartBuilder[E]]): Parser[E] = {
    
    val nameParser = simpleRefName <~ "::" ~ ws
    
    def directiveParser [E] (builder: DirectivePartBuilder[E]): Parser[E] = {
      val builderAPI = new DefaultDirectiveParserBuilder
      val (parserBuilder, directivePart) = builder(builderAPI)
      parserBuilder.parser.evalMap { parts =>
        val parsed = ParsedDirective(parts, recBlocks, recSpans)
        directivePart.apply(parsed)
      }
    }
    
    nameParser >> { name => 
      directive(provider(name.toLowerCase).map(directiveParser).getOrElse(failure(s"unknown directive: $name")), s"$name::")
    }
  }
  
  private case class InvalidDirective (msg: String, source: String, options: Options = NoOpt) extends Block with Span {
    type Self = InvalidDirective
    def withOptions (options: Options): InvalidDirective = copy(options = options)
  }
  
  private def directive [E](p: Parser[E], name: String): Parser[E] = p.handleErrorWith { f => 
    indentedBlock().map { block =>
      InvalidDirective(f.message, s".. $name " + block).asInstanceOf[E]
    }
  }

  /** Parses a role directive.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles]].
   */
  lazy val roleDirective: Parser[Block] = {
    
    val nameParser = "role::" ~ ws ~> simpleRefName ~ opt("(" ~> simpleRefName <~ ")")
    
    def directiveParser (name: String)(builder: RoleDirectivePartBuilder[String => Span]): Parser[Block] = {
      val delegate = new DefaultDirectiveParserBuilder
      val builderAPI = new DefaultRoleDirectiveParserBuilder(delegate)
      val (parserBuilder, directivePart) = builder(builderAPI)
      parserBuilder.parser.evalMap { parts =>
        val parsed = ParsedDirective(parts, recBlocks, recSpans)
        directivePart.apply(parsed).map(CustomizedTextRole(name, _))
      }
    }
    
    nameParser >> { case name ~ baseName =>
      val base = baseName.getOrElse(defaultTextRole)
      val fullName = s"role::$name" + (baseName map ("("+_+")") getOrElse "")
      directive(textRoles.get(base.toLowerCase).map(directiveParser(name))
          .getOrElse(failure(s"unknown text role: $base")), fullName).map(replaceInvalidDirective)
    }
    
  }
  
  private case class DefaultDirectiveParserBuilder (
    requiredArgs: Int = 0,
    requiredArgWithWS: Boolean = false,
    optionalArgs: Int = 0,
    optionalArgWithWS: Boolean = false,
    requiredFields: Set[String] = Set.empty,
    optionalFields: Set[String] = Set.empty,
    hasBody: Boolean = false) extends DirectiveParserBuilder {

    lazy val parser: Parser[Vector[Part]] = {
      val reqArgs = (0 until requiredArgs).map(num => arg.map(Part(Key.Argument(1, num), _)))
      val optArgs = (0 until optionalArgs).map(num => opt(arg).map(_.map(Part(Key.Argument(3, num), _))))

      val reqArgsWithWS = if (requiredArgWithWS) Seq(argWithWS.map(Part(Key.Argument(2, 0), _))) else Nil
      val optArgsWithWS = if (optionalArgWithWS) Seq(opt(argWithWS).map(_.map(Part(Key.Argument(4, 0), _)))) else Nil
      
      val allReqArgs = (reqArgs ++ reqArgsWithWS).foldLeft[Parser[Vector[Part]]](success(Vector())) {
        case (acc, p) => (acc ~ p).map { case parts ~ part => parts :+ part }
      }
      val allOptArgs = (optArgs ++ optArgsWithWS).foldLeft[Parser[Vector[Part]]](success(Vector())) {
        case (acc, p) => (acc ~ p).map { case parts ~ part => parts ++ part.toSeq }
      }
 
      val fields = if (requiredFields.nonEmpty || optionalFields.nonEmpty) directiveFieldList else success(Vector())
      
      val separator = 
        if (requiredArgWithWS || requiredArgs > 0 || requiredFields.nonEmpty) contentSeparator 
        else opt(contentSeparator)

      val bodyWithSeparator = if (hasBody) (separator ~> bodyParser).map(res => Vector(Part(Key.Body, res))) else success(Vector())

      (allReqArgs ~ allOptArgs ~ fields ~ bodyWithSeparator).map {
        case parts1 ~ parts2 ~ parts3 ~ parts4 => parts1 ++ parts2 ++ parts3 ++ parts4
      } 
    }
    
    def requiredArg (p: => Parser[String]): Parser[String] = p.withFailureMessage("missing required argument")

    val arg: Parser[String] = requiredArg(someNot(' ','\n') <~ ws)

    val argWithWS: Parser[String] = {
      val p = indentedBlock(linePredicate = not(":"), endsOnBlankLine = true).evalMap { block =>
        val text = block.trim
        if (text.nonEmpty) Right(text) else Left("missing required argument")
      }
      requiredArg(p)
    }

    val bodyParser: Parser[String] = prevIn('\n') ~> indentedBlock(firstLineIndented = true) | indentedBlock()

    // TODO - some duplicate logic with original fieldList parser
    lazy val directiveFieldList: Parser[Vector[Part]] = {

      val nameParser = ":" ~> escapedUntil(':') <~ (lookAhead(eol).as("") | " ")

      val item = ws.min(1).count >> { firstIndent =>
        nameParser ~ indentedBlock(firstIndent + 1).trim
      }

      ((opt(wsEol) ~> (item +)) | success(Nil)).evalMap { fields =>

        // TODO - 0.14 - might defer validation to a later step
        
        val parsed = fields.map(_._1).toSet

        val unknown = parsed.diff(requiredFields).diff(optionalFields)
        val missing = requiredFields.diff(parsed)
        
        def parts: Vector[Part] = fields.map { case name ~ value => Part(Key.Field(name), value) }.toVector

        val errors = 
          (if (unknown.nonEmpty) Seq(unknown.mkString("unknown options: ",", ","")) else Nil) ++ 
          (if (missing.nonEmpty) Seq(missing.mkString("missing required options: ",", ","")) else Nil)
        Either.cond(errors.isEmpty, parts, errors mkString "; ")
      }
    }

    val contentSeparator: Parser[Unit] =
      ((prevIn('\n') | eol) ~ blankLine).void | failure("blank line required to separate arguments and/or options from the body")

    def argument (withWS: Boolean = false): (Key, DirectiveParserBuilder) =
      if (withWS) (Key.Argument(2, 0), copy(requiredArgWithWS = true))
      else (Key.Argument(1, requiredArgs), copy(requiredArgs = requiredArgs + 1))

    def optArgument (withWS: Boolean = false): (Key, DirectiveParserBuilder) =
      if (withWS) (Key.Argument(4, 0), copy(optionalArgWithWS = true))
      else (Key.Argument(3, optionalArgs), copy(optionalArgs = optionalArgs + 1))

    def field (name: String): (Key, DirectiveParserBuilder) = 
      (Key.Field(name), copy(requiredFields = requiredFields + name))

    def optField (name: String): (Key, DirectiveParserBuilder) = 
      (Key.Field(name), copy(optionalFields = optionalFields + name))

    def body: (Key, DirectiveParserBuilder) =
      (Key.Body, copy(hasBody = true))
  }
  
  private class DefaultRoleDirectiveParserBuilder (val delegate: DirectiveParserBuilder) extends RoleDirectiveParserBuilder {
    
    def parser: Parser[Vector[Part]] = delegate.parser
    
    private def rewrap (res: (Key, DirectiveParserBuilder)): (Key, RoleDirectiveParserBuilder) = 
      (res._1, new DefaultRoleDirectiveParserBuilder(res._2))
    
    def field (name: String): (Key, RoleDirectiveParserBuilder) = rewrap(delegate.field(name))
    
    def optField (name: String): (Key, RoleDirectiveParserBuilder) = rewrap(delegate.optField(name))
    
    def body: (Key, RoleDirectiveParserBuilder) = rewrap(delegate.body)
    
  }
    
  
}

/** Provides the parsers for all types of extensions (directives and text roles).
  *
  * @author Jens Halm
  */
object ExtensionParsers {

  type Result[+A] = Either[String, A]

  /** Creates a new parser builder based on the specified extensions.
    */
  def allBlocks(blockDirectives: Seq[Directive[Block]],
                spanDirectives: Seq[Directive[Span]],
                textRoles: Seq[TextRole],
                defaultTextRole: String): BlockParserBuilder = BlockParser.recursive { recParsers =>
    new ExtensionParsers(recParsers,
      RstExtension.createAsMap(blockDirectives, recParsers),
      RstExtension.createAsMap(spanDirectives, recParsers),
      RstExtension.createAsMap(textRoles, recParsers),
      defaultTextRole).explicitBlockItem
  }
}
