package laika.parse.rst

import laika.tree.Elements._
import Elements.SubstitutionReference

trait InlineParsers extends laika.parse.InlineParsers {

  
  val pairs = Map(/* Ps/Pe pairs */
              '('->')', '['->']', '{'->'}', '\u0f3a'->'\u0f3b', '\u0f3c'->'\u0f3d', '\u169b'->'\u169c', '\u2045'->'\u2046',
                  '\u207d'->'\u207e', '\u208d'->'\u208e', '\u2329'->'\u232a', '\u2768'->'\u2769', '\u276a'->'\u276b',
                  '\u276c'->'\u276d', '\u276e'->'\u276f', '\u2770'->'\u2771', '\u2772'->'\u2773', '\u2774'->'\u2775',
                  '\u27c5'->'\u27c6', '\u27e6'->'\u27e7', '\u27e8'->'\u27e9', '\u27ea'->'\u27eb', '\u27ec'->'\u27ed',
                  '\u27ee'->'\u27ef', '\u2983'->'\u2984', '\u2985'->'\u2986', '\u2987'->'\u2988', '\u2989'->'\u298a',
                  '\u298b'->'\u298c', '\u298d'->'\u298e', '\u298f'->'\u2990', '\u2991'->'\u2992', '\u2993'->'\u2994',
                  '\u2995'->'\u2996', '\u2997'->'\u2998', '\u29d8'->'\u29d9', '\u29da'->'\u29db', '\u29fc'->'\u29fd',
                  '\u2e22'->'\u2e23', '\u2e24'->'\u2e25', '\u2e26'->'\u2e27', '\u2e28'->'\u2e29', '\u3008'->'\u3009',
                  '\u300a'->'\u300b', '\u300c'->'\u300d', '\u300e'->'\u300f', '\u3010'->'\u3011', '\u3014'->'\u3015',
                  '\u3016'->'\u3017', '\u3018'->'\u3019', '\u301a'->'\u301b', '\u301d'->'\u301e', '\ufd3e'->'\ufd3f',
                  '\ufe17'->'\ufe18', '\ufe35'->'\ufe36', '\ufe37'->'\ufe38', '\ufe39'->'\ufe3a', '\ufe3b'->'\ufe3c',
                  '\ufe3d'->'\ufe3e', '\ufe3f'->'\ufe40', '\ufe41'->'\ufe42', '\ufe43'->'\ufe44', '\ufe47'->'\ufe48',
                  '\ufe59'->'\ufe5a', '\ufe5b'->'\ufe5c', '\ufe5d'->'\ufe5e', '\uff08'->'\uff09', '\uff3b'->'\uff3d',
                  '\uff5b'->'\uff5d', '\uff5f'->'\uff60', '\uff62'->'\uff63', 
                  /* Pi/Pf pairs */
                  '\u00ab'->'\u00bb', '\u2018'->'\u2019', '\u201c'->'\u201d', '\u2039'->'\u203a', '\u2e02'->'\u2e03',
                  '\u2e04'->'\u2e05', '\u2e09'->'\u2e0a', '\u2e0c'->'\u2e0d', '\u2e1c'->'\u2e1d', '\u2e20'->'\u2e21',
                  /* Pi/Pf pairs reverse */
                  '\u00bb'->'\u00ab', '\u2019'->'\u2018', '\u201d'->'\u201c', '\u203a'->'\u2039', '\u2e03'->'\u2e02',
                  '\u2e05'->'\u2e04', '\u2e0a'->'\u2e09', '\u2e0d'->'\u2e0c', '\u2e1d'->'\u2e1c', '\u2e21'->'\u2e20',
                  /* pairs added explicitly in the reStructuredText ref impl */
                  '\u301d'->'\u301f', '\u201a'->'\u201b', '\u201e'->'\u201f', '\u201b'->'\u201a', '\u201f'->'\u201e',
                  /* additional pairing of open/close quotes for different typographic conventions in different languages */
                  '\u00bb'->'\u00bb', '\u2018'->'\u201a', '\u2019'->'\u2019', '\u201a'->'\u2018', '\u201a'->'\u2019',
                  '\u201c'->'\u201e', '\u201e'->'\u201c', '\u201e'->'\u201d', '\u201d'->'\u201d', '\u203a'->'\u203a')

                  
  val startChars = anyOf(' ','-',':','/','\'','"','<','(','[','{') take 1
  
  val startCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.START_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)
   
  val endChars = anyOf(' ','-','.',',',':',';','!','?','\\','/','\'','"','>',')',']','}','\u201a','\u201e') take 1
  
  val endCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.END_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)                          

                            
  // TODO - eliminate escaped spaces before markup start
  def markupStart (start: Parser[Any], end: Parser[String]): Parser[Any] = {
    (lookBehind(2, (atStart ^^^ ' ') | beforeStartMarkup) >> afterStartMarkup(start)) ~ not(end) // not(end) == rule 6
  }
  
  def markupStart (end: Parser[String]): Parser[Any] = markupStart(success(()), end)
  
  def markupEnd (end: Parser[String]) = {
    end >> { markup => lookBehind(markup.length, beforeEndMarkup) ~ guard(eof | afterEndMarkup) }
  }
  
  // TODO - promote to MarkupParsers
  val atStart = Parser { in =>
    if (in.offset == 0) Success(success(()), in) 
    else Failure("Not at start of input", in)
  }
  
  /** Inline markup recognition rules 2 and 5
   */
  def afterStartMarkup (start: Parser[Any])(before: Char) = {
    val matching = pairs.getOrElse(before, ' ') 
    start ~ guard(anyBut(' ','\n', matching) take 1)
  }
  
  /** Inline markup recognition rules 3
   */
  val beforeEndMarkup = anyBut(' ','\n') take 1
  
  /** Inline markup recognition rule 1
   */
  val beforeStartMarkup = startChars ^^ {_.charAt(0)} | acceptIf(char => startCategories(Character.getType(char)))("Not a start char: " + _)
  
  /** Inline markup recognition rule 4
   */
  val afterEndMarkup = endChars | acceptIf(char => endCategories(Character.getType(char)))("Not an end char: " + _)
  
  
  val spanParsers = Map(
    '*' -> (strong | em),   
    '`' -> inlineLiteral,
    '[' -> (footnoteRef | citationRef),
    '|' -> substitutionRef,
    '_' -> internalTarget
  )

  
  val em = span("*") ^^ Emphasized
  
  val strong = span('*',"**") ^^ Strong
  

  def span (start: Parser[Any], end: Parser[String]): Parser[List[Span]]
    = markupStart(start, end) ~> spans(anyUntil(markupEnd(end)), spanParsers)
    
  def span (end: Parser[String]): Parser[List[Span]] = span(success(()), end)
    
  
  lazy val inlineLiteral = markupStart('`', "``") ~> anyUntil(markupEnd("``")) ^^ CodeSpan
  
  
  val simpleRefName = {
    val alphanum = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1 // TODO - check whether non-ascii characters are allowed
    val symbol = anyOf('-', '_', '.', ':', '+') take 1
    
    alphanum ~ ((symbol ~ alphanum)*) ^^ { 
      case start ~ rest => start + (rest map { case a~b => a+b }).mkString
    } // TODO - normalize ws - lowercase
  }
  
  val refName = simpleRefName | phraseRef
  
  val phraseRef = '`' ~> (anyBut('`') min 1) <~ '`'
  
  
  val footnoteLabel = {
    val decimal = (anyIn('0' to '9') min 1) ^^ { n => NumericLabel(n.toInt) }
    val autonumber = '#' ^^^ Autonumber 
    val autosymbol = '*' ^^^ Autosymbol
    val autonumberLabel = '#' ~> simpleRefName ^^ AutonumberLabel 
    
    decimal | autonumberLabel | autonumber | autosymbol
  }
  
  lazy val footnoteRef = markupStart("]_") ~> footnoteLabel <~ markupEnd("]_") ^^ FootnoteReference
  
  lazy val citationRef = markupStart("]_") ~> simpleRefName <~ markupEnd("]_") ^^ CitationReference
  
  lazy val substitutionRef = markupStart("|") ~> simpleRefName <~ markupEnd("|") ^^ SubstitutionReference
  
  
  val internalTarget = markupStart('`', "`") ~> (anyBut('`') min 1) <~ markupEnd("`") ^^ InlineLinkTarget // TODO - normalize id's?
  
  
}