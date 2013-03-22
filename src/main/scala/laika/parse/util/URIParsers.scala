package laika.parse.util

import laika.parse.MarkupParsers

trait URIParsers extends MarkupParsers {

  
  def flatten (result: Any): String = result match {
    case Some(x)      => flatten(x)
    case None         => ""
    case c: Char      => c.toString
    case s: String    => s
    case l: List[Any] => l.map(flatten).mkString
    case x ~ y        => flatten(x) + flatten(y)
  }
  

  val alpha = anyIn('a' to 'z', 'A' to 'Z')
  
  val digit = anyIn('0' to '9')
  
  val hexdig = anyIn('0' to '9', 'A' to 'F')
  
  
  val subDelims = anyOf('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=') take 1
  
  val unreserved = (alpha take 1) | (digit take 1) | (anyOf('-', '.', '_', '~') take 1)
  
  val pctEncoded = '%' ~ (hexdig take 2)
  
  
  /* authority */  
  
  val ipvFuture = 'v' ~ (hexdig min 1) ~ '.' ~ ((unreserved | subDelims | ':')+)
  
  val ipv4address = {
    val decOctet = (digit min 1 max 3) ^^? { res => 
      val num = res.toInt
      if (num > 0 && num < 256) Right(num) else Left("Number must be between 1 and 255")
    }
    
    decOctet ~ repN(3, '.' ~ decOctet) ^^ flatten
  }
  
  val ipv6address = {
    
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
  
  val ipLiteral = '[' ~ (ipv6address | ipvFuture) ~ ']' ^^ flatten
  
  val regName = ((unreserved | pctEncoded | subDelims)*) ^^ flatten
  
  val host = ipLiteral | ipv4address | regName
  
  val port = digit min 1
  
  val userInfo = ((unreserved | pctEncoded | subDelims | ':')*) ^^ flatten
  
  val authority = opt(userInfo ~ '@') ~ host ~ opt(':' ~ port) ^^ flatten
  

  /* path */                  
                    
  val pChar = (unreserved | pctEncoded | subDelims | (anyOf(':','@') take 1))                  
                    
  val path = {
    val segment = '/' ~ (pChar*)
    (segment*) ^^ flatten  
  }
  
  val query = ((pChar | (anyOf('/','?') take 1))*) ^^ flatten
      
  val fragment = query

  
  /* uri */
  
  val hierPart = ("//" ~ authority ~ path) ^^ flatten
  
  val httpUriNoScheme = hierPart ~ opt('?' ~ query) ~ opt('#' ~ fragment) ^^ flatten
  
  val httpUri = "http:" ~ httpUriNoScheme ^^ flatten

  val httpsUri = "https:" ~ httpUriNoScheme ^^ flatten
  
  
  /* email */
  
  val dotAtomText = {
    
    val atext = (alpha min 1) | (digit min 1) | 
        (anyOf('!','#','$','%','&',''','*','+','-','/','=','?','^','_','`','{','|','}','~') min 1)
        
    ((atext*) ~ (('.' ~ (atext*))*)) ^^ flatten 
  }

  /* Parses the local part of an email address (before the @), with one 
   * deviation from RFC 6068: a quoted string is not allowed. It is rarely
   * used, not supported by the reStructuredText reference parser and would
   * be hard to combine within text markup as it allows for whitespace and
   * line break characters.
   */
  val localPart = dotAtomText 
  
  val domain = {
    val dtextNoObs = anyIn('!' to 'Z', '^' to '~') // all printable ASCII except "[", "]", "\"
    
    (dotAtomText | ('[' ~ dtextNoObs ~ ']')) ^^ flatten
  }   
  
  val addrSpec = (localPart ~ '@' ~ domain) ^^ flatten
  
  val to = (addrSpec ~ ((',' ~ addrSpec)*)) ^^ flatten
  
  val hfields = {
        
    val someDelims = anyOf('!', '$', '\'', '(', ')', '*', '+', ',', ';', ':', '@') min 1
    val qChar = unreserved | pctEncoded | someDelims
        
    val hfname  = qChar*
    val hfvalue = qChar*

    val hfield = hfname ~ '=' ~ hfvalue
    
    ('?' ~ hfield ~ (('&' ~ hfield)*)) ^^ flatten
  }
  
  val emailAddress = to ~ opt(hfields) ^^ flatten
  
  val emailURI = "mailto:" ~ opt(emailAddress) ^^ flatten
  
}