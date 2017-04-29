/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.util

import laika.parse.MarkupParsers
import laika.parse.core.{Parser, ~}

/**
 * Parses URIs as defined in RFC 3986 and email addresses as defined in
 * RFC 6068, 5322 and 3986 with base types defined in RFC 2234. 
 * 
 * @author Jens Halm
 */
trait URIParsers extends MarkupParsers {

  
  /** Flattens the result from various combinators,
   *  including the `repX` variants and `~` into 
   *  a single string.
   */
  def flatten (result: Any): String = result match {
    case Some(x)      => flatten(x)
    case None         => ""
    case i: Int       => i.toString
    case c: Char      => c.toString
    case s: String    => s
    case l: List[_]   => l.map(flatten).mkString
    case x ~ y        => flatten(x) + flatten(y)
  }
  

  /** Parses letters according to RFC 2234.
   * 
   *  {{{
   *  ALPHA =  %x41-5A / %x61-7A ; A-Z / a-z
   *  }}}
   */
  val alpha: TextParser = anyIn('a' to 'z', 'A' to 'Z')

  /** Parses digits according to RFC 2234.
   * 
   *  {{{
   *  DIGIT =  %x30-39; 0-9
   *  }}}
   */
  val digit: TextParser = anyIn('0' to '9')

  /** Parses a hexadecimal value according to RFC 2234.
   * 
   *  {{{
   *  HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
   *  }}}
   */
  val hexdig: TextParser = anyIn('0' to '9', 'A' to 'F')
  
  /** Parses a single sub-delimiter as defined in RFC 3986.
   * 
   *  {{{
   *  sub-delims = "!" / "$" / "&" / "'" / "(" / ")"
   *                   / "*" / "+" / "," / ";" / "="
   *  }}}
   */
  val subDelims: Parser[String] = anyOf('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=') take 1
  
  /** Parses a single unreserved character as defined in RFC 3986.
   * 
   *  {{{
   *  sub-delims = "!" / "$" / "&" / "'" / "(" / ")"
   *                   / "*" / "+" / "," / ";" / "="
   *  }}}
   */
  val unreserved: Parser[String] = (alpha take 1) | (digit take 1) | (anyOf('-', '.', '_', '~') take 1)
  
  /** Parses a percent-encoded character as defined in RFC 3986.
   * 
   *  {{{
   *  pct-encoded = "%" HEXDIG HEXDIG
   *  }}}
   */
  val pctEncoded: Parser[Char ~ String] = '%' ~ (hexdig take 2)
  
  
  /* authority */  
  
  /** Parses a future IP address as defined in RFC 3986.
   * 
   *  {{{
   *  IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
   *  }}}
   */
  val ipvFuture: Parser[Char ~ String ~ Char ~ List[Any]] = 'v' ~ (hexdig min 1) ~ '.' ~ ((unreserved | subDelims | ':')+)
  
  /** Parses an IPv4 address as defined in RFC 3986.
   * 
   *  {{{
   *  IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
   * 
   *  dec-octet     = DIGIT                 ; 0-9
   *                / %x31-39 DIGIT         ; 10-99
   *                / "1" 2DIGIT            ; 100-199
   *                / "2" %x30-34 DIGIT     ; 200-249
   *                / "25" %x30-35          ; 250-255
   *  }}}
   *  The implementation has been simplified to parse a 3-digit number and
   *  check its value.
   */
  val ipv4address: Parser[String] = {
    val decOctet = (digit min 1 max 3) ^^? { res => 
      val num = res.toInt
      if (num >= 0 && num < 256) Right(num) else Left("Number must be between 1 and 255")
    }
    
    decOctet ~ repN(3, '.' ~ decOctet) ^^ flatten
  }
  
  /** Parses an IPv6 address as defined in RFC 3986.
   * 
   *  {{{
   *  IPv6address  =                            6( h16 ":" ) ls32
   *               /                       "::" 5( h16 ":" ) ls32
   *               / [               h16 ] "::" 4( h16 ":" ) ls32
   *               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
   *               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
   *               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
   *               / [ *4( h16 ":" ) h16 ] "::"              ls32
   *               / [ *5( h16 ":" ) h16 ] "::"              h16
   *               / [ *6( h16 ":" ) h16 ] "::"
   *       
   *  h16          = 1*4HEXDIG
   * 
   *  ls32         = ( h16 ":" h16 ) / IPv4address
   *  }}}
   */
  val ipv6address: Parser[Any ~ Any] = {
    
    val h16 = hexdig min 1 max 4
  
    val h16Col = h16 ~ ':'
  
    val ls32 = (h16 ~ ':' ~ h16) | ipv4address
  
    (repN(6, h16Col) ~ ls32) |
    ("::" ~ repN(5, h16Col) ~ ls32) |
    (opt(h16) ~ "::" ~ repN(4, h16Col) ~ ls32) |
    (opt(repMax(1, h16Col) ~ h16) ~ "::" ~ repN(3, h16Col) ~ ls32) |
    (opt(repMax(2, h16Col) ~ h16) ~ "::" ~ repN(2, h16Col) ~ ls32) |
    (opt(repMax(3, h16Col) ~ h16) ~ "::" ~ h16Col ~ ls32) |
    (opt(repMax(4, h16Col) ~ h16) ~ "::" ~ ls32) |
    (opt(repMax(5, h16Col) ~ h16) ~ "::" ~ h16) |
    (opt(repMax(6, h16Col) ~ h16) ~ "::")
  }
  
  /** Parses an ip literal as defined in RFC 3986.
   * 
   *  {{{
   *  IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
   *  }}}
   */
  val ipLiteral: Parser[String] = '[' ~ (ipv6address | ipvFuture) ~ ']' ^^ flatten
  
  /** Parses a server name as defined in RFC 3986.
   * 
   *  {{{
   *  reg-name = *( unreserved / pct-encoded / sub-delims )
   *  }}}
   */
  val regName: Parser[String] = ((unreserved | pctEncoded | subDelims)*) ^^ flatten
  
  /** Parses a host as defined in RFC 3986.
   * 
   *  {{{
   *  host = IP-literal / IPv4address / reg-name
   *  }}}
   */
  val host: Parser[String] = ipLiteral | ipv4address | regName
  
  /** Parses a port as defined in RFC 3986, except for requiring at least one digit;
   *  instead the port is defined as optional in a higher level combinator.
   * 
   *  {{{
   *  port = *DIGIT
   *  }}}
   */
  val port: Parser[String] = digit min 1
  
  /** Parses the user info portion of a URI as defined in RFC 3986.
   * 
   *  {{{
   *  userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
   *  }}}
   */
  val userInfo: Parser[String] = ((unreserved | pctEncoded | subDelims | ':')*) ^^ flatten
  
  /** Parses the authority part of a URI as defined in RFC 3986.
   * 
   *  {{{
   *  authority = [ userinfo "@" ] host [ ":" port ]
   *  }}}
   */
  val authority: Parser[String] = opt(userInfo ~ '@') ~ host ~ opt(':' ~ port) ^^ flatten
  

  /* path */                  
  
  /** Parses one path character as defined in RFC 3986.
   * 
   *  {{{
   *  pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
   *  }}}
   */
  val pChar: Parser[Any] = unreserved | pctEncoded | subDelims | (anyOf(':', '@') take 1)
    
  /** Parses the path of a URI as defined in RFC 3986, but only the path
   *  variant following an authority component.
   * 
   *  {{{
   *  path-abempty  = *( "/" segment )
   * 
   *  segment       = *pchar
   *  }}}
   */
  val path: Parser[String] = {
    val segment = '/' ~ (pChar*)
    (segment*) ^^ flatten  
  }

  /** Parses the query part of a URI as defined in RFC 3986.
   * 
   *  {{{
   *  query = *( pchar / "/" / "?" )
   *  }}}
   */
  val query: Parser[String] = ((pChar | (anyOf('/','?') take 1))*) ^^ flatten
  
  /** Parses the fragment part of a URI as defined in RFC 3986.
   * 
   *  {{{
   *  fragment = *( pchar / "/" / "?" )
   *  }}}
   */  
  val fragment: Parser[String] = query

  
  /* uri */
  
  /** Parses the hierarchical part of a URI with an authority component as defined in RFC 3986,
   *  but only the variant including an authority component.
   * 
   *  {{{
   *  hier-part     = "//" authority path-abempty
   *                / path-absolute ; excluded
   *                / path-rootless ; excluded
   *                / path-empty    ; excluded
   *  }}}
   */  
  val hierPart: Parser[String] = ("//" ~ authority ~ path) ^^ flatten
  
  /** Parses an HTTP or HTTPS URI with an authority component, but without the scheme part 
   *  (therefore starting with "//") as defined in RFC 3986.
   *  
   *  {{{
   *  URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
   *  }}}
   */
  val httpUriNoScheme: Parser[String] = hierPart ~ opt('?' ~ query) ~ opt('#' ~ fragment) ^^ flatten

  /** Parses a full HTTP URI including the scheme part and an authority component 
   *  as defined in RFC 3986.
   */
  val httpUri: Parser[String] = "http:" ~ httpUriNoScheme ^^ flatten

  /** Parses a full HTTPS URI including the scheme part and an authority component 
   *  as defined in RFC 3986.
   */
  val httpsUri: Parser[String] = "https:" ~ httpUriNoScheme ^^ flatten
  
  
  /* email */
  
  /** Parses a `dot-atom-text` sequence as defined in RFC 5322.
   * 
   *  {{{
   *  dot-atom-text   =   1*atext *("." 1*atext)
   *   
   *   atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
   *                    "!" / "#" /        ;  characters not including
   *                    "$" / "%" /        ;  specials.  Used for atoms.
   *                    "&" / "'" /
   *                    "*" / "+" /
   *                    "-" / "/" /
   *                    "=" / "?" /
   *                    "^" / "_" /
   *                    "`" / "{" /
   *                    "|" / "}" /
   *                    "~"
   *  }}}
   */
  val dotAtomText: Parser[String] = {
    
    val atext = (alpha min 1) | (digit min 1) | 
        (anyOf('!','#','$','%','&','\'','*','+','-','/','=','?','^','_','`','{','|','}','~') min 1)
        
    ((atext*) ~ (('.' ~ (atext*))*)) ^^ flatten 
  }

  /** Parses the local part of an email address (before the @), with one 
   *  deviation from RFC 6068: a quoted string is not allowed. It is rarely
   *  used, not supported by the reStructuredText reference parser and would
   *  be hard to combine within text markup as it allows for whitespace and
   *  line break characters.
   * 
   *  {{{
   *  local-part = dot-atom-text / quoted-string ; quoted-string omitted
   *  }}}
   */
  val localPart: Parser[String] = dotAtomText 
  
  /** Parses the domain portion of an email address as defined in RFC 6068.
   * 
   *  {{{
   *  domain       = dot-atom-text / "[" *dtext-no-obs "]"
   *  dtext-no-obs = %d33-90 / ; Printable US-ASCII
   *                 %d94-126  ; characters not including
   *                           ; "[", "]", or "\"
   *  }}} 
   */
  val domain: Parser[String] = {
    val dtextNoObs = anyIn('!' to 'Z', '^' to '~') // all printable ASCII except "[", "]", "\"
    
    (dotAtomText | ('[' ~ dtextNoObs ~ ']')) ^^ flatten
  }   
  
  /** Parses a single email address as defined in RFC 6068.
   * 
   *  {{{
   *  addr-spec = local-part "@" domain
   *  }}}
   */
  val addrSpec: Parser[String] = (localPart ~ '@' ~ domain) ^^ flatten
  
  /** Parses a sequence of email addresses as defined in RFC 6068.
   * 
   *  {{{
   *  to = addr-spec *("," addr-spec )
   *  }}}
   */
  val to: Parser[String] = (addrSpec ~ ((',' ~ addrSpec)*)) ^^ flatten
  
  /** Parses header fields of an email address as defined in RFC 6068.
   * 
   *  {{{
   *  hfields      = "?" hfield *( "&" hfield )
   *  hfield       = hfname "=" hfvalue
   *  hfname       = *qchar
   *  hfvalue      = *qchar
   * 
   *  qchar        = unreserved / pct-encoded / some-delims
   *  some-delims  = "!" / "$" / "'" / "(" / ")" / "*"
   *               / "+" / "," / ";" / ":" / "@"
   *  }}}
   */
  val hfields: Parser[String] = {
        
    val someDelims = anyOf('!', '$', '\'', '(', ')', '*', '+', ',', ';', ':', '@') min 1
    val qChar = unreserved | pctEncoded | someDelims
        
    val hfname  = qChar*
    val hfvalue = qChar*

    val hfield = hfname ~ '=' ~ hfvalue
    
    ('?' ~ hfield ~ (('&' ~ hfield)*)) ^^ flatten
  }
  
  /** Parses a mailto URI without the scheme part as defined in RFC 6068.
   */
  val emailAddress: Parser[String] = to ~ opt(hfields) ^^ flatten
  
  /** Parses a full mailto URI as defined in RFC 6068.
   * 
   *  {{{
   *  mailtoURI = "mailto:" [ to ] [ hfields ]
   *  }}}
   */
  val emailURI: Parser[String] = "mailto:" ~ opt(emailAddress) ^^ flatten
  
}
