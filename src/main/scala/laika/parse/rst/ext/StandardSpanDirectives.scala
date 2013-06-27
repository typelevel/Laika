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
  
package laika.parse.rst.ext

import laika.tree.Elements._
import laika.parse.rst.Directives._
import laika.parse.rst.Directives.Parts._
import java.text.SimpleDateFormat
import java.util.Date

/** Defines all supported standard span directives of the reStructuredText reference parser.
 *  A span directive can be used in substitution definitions.
 * 
 *  The `replace` directive is fully supported. The other directives have the following
 *  adjustments or limitations compared to their counterparts in the reference parser:
 * 
 *  - `unicode`: does not support the various trim options, as that would require modifying adjacent elements
 *    (and no other directive has this requirment, therefore API/impl changes did not seem justified)
 * 
 *  - `date`: Uses the patterns of `java.text.SimpleDateFormat` instead of Python's `time.strftime` function.
 * 
 *  - `image`: Does not support the various layout options (`width`, `height`, `scale`, `align`), as no other
 *    tree nodes in Laika carry concrete layout information. It is recommended to use styles instead. 
 * 
 *  @author Jens Halm
 */
trait StandardSpanDirectives {

  /** All custom parsers needed by the directive implementations.
   */
  val parse = new StandardDirectiveParsers {}
  
  protected val nameOpt = optField("name")
  
  protected val classOpt = optField("class")
  
  def toOptions (id: Option[String], styles: Option[String]) = Options(id, styles.map(_.split(" ").toList).getOrElse(Nil)) // TODO - merge combinators
  
  
  lazy val image: DirectivePart[Span] = {
    def multilineURI (text: String) = Right(text.split("\n").map(_.trim).mkString("\n").trim)
    
    (argument(multilineURI, withWS = true) ~ optField("alt") ~ optField("target", parse.target) ~ nameOpt ~ classOpt) { (uri, alt, target, id, styles) =>
      val image = Image(alt.getOrElse(""), uri, None, toOptions(id,styles))
      (target map { 
        case ref: ExternalLink  => ref.copy(content = List(image))
        case ref: LinkReference => ref.copy(content = List(image))
      }).getOrElse(image)
    } 
  }
  
  lazy val replace: DirectivePart[Span] = spanContent map (SpanSequence(_)) 
  
  lazy val unicode: DirectivePart[Span] = argument(parse.unicode, withWS = true) map (Text(_)) 
  
  lazy val date: DirectivePart[Span] = {
    optArgument(withWS = true) map { pattern => 
      Text((new SimpleDateFormat(pattern.getOrElse("yyyy-MM-dd")).format(new Date)))
    } 
  }
  
  /** All standard span directives currently supported by Laika.
   */
  lazy val spanDirectives = List(
    SpanDirective("image")(image),
    SpanDirective("replace")(replace),
    SpanDirective("unicode")(unicode),
    SpanDirective("date")(date)
  )
  
  
  
}