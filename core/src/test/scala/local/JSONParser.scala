package local

import scala.util.parsing.combinator.JavaTokenParsers

class JSONParser extends JavaTokenParsers {

  
  /**  value ::=  obj | arr | stringLiteral | numberLiteral | "null" | "true" | "false"
   */
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble) | 
                          "null" ^^^ null | "true" ^^^ true | "false" ^^^ false
  
  /**  arr     ::=  "[" [values] "]"
   *   values  ::=  value {"," value}
   */
  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"
  
  /**  obj     ::=  "{" [members] "}"
   *   members ::=  member {"," member}
   */
  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (_.toMap)
  
  /**  member  ::=  stringLiteral ":" value
   */
  def member: Parser[(String, Any)] = stringLiteral ~ (":" ~> value) ^^ { 
    case name ~ value => (name, value) 
  }
  
  
}

object JSON {
  
  val parser = new JSONParser
  
  def apply (input: String) = println(parser.parseAll(parser.value, input))
  
}