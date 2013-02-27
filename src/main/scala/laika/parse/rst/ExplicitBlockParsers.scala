package laika.parse.rst

import laika.parse.InlineParsers
import laika.tree.Elements._
import laika.util.Builders._
import laika.parse.rst.Elements.SubstitutionDefinition
import Directives._

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
  def nestedBlocks (pos: BlockPosition) = 
    if (pos.nestLevel < maxNestLevel) (standardRstBlock(pos) | paragraph) *
    else (nonRecursiveRstBlock | paragraph) *
  
  
  def explicitStart = (".." ~ (ws min 1)) ^^ { case _ ~ ws => ws.length + 2 }
  
  
  def explicitBlockItems (pos: BlockPosition) = explicitStart >> { len => // TODO - length not needed when tab processing gets changed
    footnote(pos, len) | citation(pos, len) | linkDefinition(pos, len) | substitutionDefinition(pos, len) // TODO - there is a linkDef alternative not requiring the .. prefix
  }
  
  
  def footnote (pos: BlockPosition, prefixLength: Int) = {
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
      indentedBlock(prefix ^^ labelLength, pos) ^^ {
        case (lines,pos) => Footnote(label, parseMarkup(nestedBlocks(pos), lines mkString "\n"))
      }
    }
  }
  
  def citation (pos: BlockPosition, prefixLength: Int) = {
    val prefix = '[' ~> simpleRefName <~ ']'
    
    guard(prefix) >> { label => // TODO - parsing prefix twice is inefficient, indentedBlock parser should return result
      indentedBlock(prefix ^^ { _.length + prefixLength }, pos) ^^ {
        case (lines,pos) => Citation(label, parseMarkup(nestedBlocks(pos), lines mkString "\n"))
      }
    }
  }
  
  def linkDefinition (pos: BlockPosition, prefixLength: Int) = {
    val named = '_' ~> refName <~ ':' // TODO - backticks are optional here in most cases (unless ref name contains colons)
    val anonymous = "__:"
      
    val internal = named ^^ InternalLinkTarget // TODO - might need special logic for cases where it points to other targets (e.g. external)
    
    val externalName = (named | anonymous)
    
    val external = guard(externalName) >> { name =>
      indentedBlock(externalName ^^ { _.length + prefixLength }, pos) ^^ { // TODO - indentedBlock parser still requires ws after prefix
        case (lines,pos) => LinkDefinition(name, lines map (_.trim) filter (_.isEmpty) mkString)
      }
    }
    
    external | internal
    
    // TODO - add indirect targets once inline parsers are completed
  }
  
  def substitutionDefinition (pos: BlockPosition, prefixLength: Int) = {
    val text = not(ws take 1) ~> (anyBut('|','\n') min 1)  
    val prefix = '|' ~> text <~ not(lookBehind(1, ' ')) ~ '|'
    
    ((prefix <~ ws) ~ spanDirective) ^^ { case name ~ content => SubstitutionDefinition(name, content) }
  }
  
  def comment (pos: BlockPosition, prefixLength: Int) = {
    indentedBlock(success(prefixLength), pos) ^^ { // TODO - indentedBlock parser still requires ws after prefix
      case (lines,pos) => Comment(lines map (_.trim) filter (_.isEmpty) mkString)
    }
  }
  
  
  val blockDirectives: Map[String, DirectivePart[Seq[Block]]] // TODO - populate (maybe do this in reStructuredText entry point object)
  
  val spanDirectives: Map[String, DirectivePart[Seq[Span]]]
    

  def blockDirective = directive(blockDirectives)

  def spanDirective = directive(spanDirectives)
  
  def directive [E](directives: Map[String, DirectivePart[Seq[E]]]) = {
    
    val nameParser = simpleRefName <~ "::" ~ ws
    
    nameParser >> { name =>
      
      val directive = directives(name) // TODO - handle unknown names
      
      val parserBuilder = new DirectiveParserBuilder
      
      val result = directive(parserBuilder)
      
      parserBuilder.parser ^^^ {
        result.get
      }
    }
    
  }
  
  // TODO - deal with failures and exact behaviour for unknown directives and other types of error
  class DirectiveParserBuilder extends DirectiveParser {

    def parser: Parser[Any] = requiredArgs ~ optionalArgs // TODO - add fields and body
    
    val arg = (anyBut(' ') min 1) <~ ws
    
    def field (name: String) = ':' ~ name ~ ':' ~ ws ~> (anyBut(' ') min 1) <~ ws // TODO - refine
    
    var requiredArgs: Parser[Any] = success(()) // TODO - integrate lastArgWhitespace option
    var optionalArgs: Parser[Any] = success(())
    var contentParser: Parser[Any] = success(())
    
    val requiredFields: Map[String,Parser[Any]] = Map.empty
    val optionalFields: Map[String,Parser[Any]] = Map.empty
    
    class LazyResult[T] {
      var value: Option[T] = None
      def set (v: T) = value = Some(v)
      def get: T = value getOrElse { throw new IllegalStateException("result not set yet") }
      def fromString (f: String => T)(s: String) = set(f(s))
    }
    
    def requiredArg [T](f: String => T): Result[T] = {
      val result = new LazyResult[T]
      requiredArgs = requiredArgs ~ (arg ^^ result.fromString(f))
      new Result(result.get)
    }
    
    def optionalArg [T](f: String => T): Result[Option[T]] = {
      val result = new LazyResult[T]
      optionalArgs = optionalArgs ~ (arg ^^ result.fromString(f))
      new Result(result.value)
    }
    
    def requiredField [T](name: String, f: String => T): Result[T] = {
      val result = new LazyResult[T]
      requiredFields + (name -> (field(name) ^^ result.fromString(f)))
      new Result(result.get)
    }
    
    def optionalField [T](name: String, f: String => T): Result[Option[T]] = {
      val result = new LazyResult[T]
      optionalFields + (name -> (field(name) ^^ result.fromString(f)))
      new Result(result.value)
    }
    
    def standardContent: Result[Seq[Block]] = content { rawtext =>
      parseMarkup(nestedBlocks(new BlockPosition(0,0)), rawtext) /* TODO - consider passing position to function 
                                                                    (BlockPosition is currently a path dependent type, 
                                                                    inconvenient for top level API integration */
    }
    
    def content [T](f: String => T): Result[T] = {
      val result = new LazyResult[T]
      contentParser = (success("") ^^ result.fromString(f)) // TODO - implement
      new Result(result.get)
    }
    
  }
    
  
}