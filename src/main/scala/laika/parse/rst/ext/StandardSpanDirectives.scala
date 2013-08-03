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
 *    (and no other directive has this requirement, therefore API/impl changes did not seem justified)
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
  
  /** The name option which is supported by almost all reStructuredText directives.
   */
  protected val nameOpt = optField("name")
  
  /** The class option which is supported by almost all reStructuredText directives.
   */
  protected val classOpt = optField("class")
  
  /** The standard class and name options supported by most directives,
   *  combined in the result into an Options instance.
   */
  protected val stdOpt = (nameOpt ~ classOpt) { (id, styles) => toOptions(id, styles) } 
  
  /** Converts an optional id and an optional style parameter containing
   *  a space-delimited list of styles to an `Options` instance.
   */
  protected def toOptions (id: Option[String], styles: Option[String]) = 
    Options(id, styles.map(_.split(" ").toList).getOrElse(Nil))
  
  
  /** The image directive for span elements, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
   */
  lazy val image: DirectivePart[Span] = {
    def multilineURI (text: String) = Right(text.split("\n").map(_.trim).mkString("\n").trim)
    
    (argument(multilineURI, withWS = true) ~ optField("alt") ~ optField("target", parse.target) ~ stdOpt) { (uri, alt, target, opt) =>
      val image = Image(alt.getOrElse(""), uri, None, opt)
      (target map { 
        case ref: ExternalLink  => ref.copy(content = List(image))
        case ref: LinkReference => ref.copy(content = List(image))
      }).getOrElse(image)
    } 
  }
  
  /** The replace directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#replacement-text]] for details.
   */
  lazy val replace: DirectivePart[Span] = spanContent map (SpanSequence(_)) 
  
  /** The unicode directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#unicode-character-codes]] for details.
   */
  lazy val unicode: DirectivePart[Span] = argument(parse.unicode, withWS = true) map (Text(_)) 
  
  /** The date directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#date]] for details.
   */
  lazy val date: DirectivePart[Span] = {
    optArgument(withWS = true) map { pattern => 
      Text((new SimpleDateFormat(pattern.getOrElse("yyyy-MM-dd")).format(new Date)))
    } 
  }
  
  /** All standard reStrucuturedText span directives,
   *  to be used in substitution references.
   */
  lazy val spanDirectives = List(
    SpanDirective("image")(image),
    SpanDirective("replace")(replace),
    SpanDirective("unicode")(unicode),
    SpanDirective("date")(date)
  )
  
  
  
}