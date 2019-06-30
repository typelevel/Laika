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

package laika.rst.std

import java.text.SimpleDateFormat
import java.util.Date

import laika.ast._
import laika.rst.ext.Directives.Parts._
import laika.rst.ext.Directives._

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
class StandardSpanDirectives {


  /** The replace directive,
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#replacement-text]] for details.
   */
  lazy val replace: DirectivePartBuilder[Span] = spanContent map (SpanSequence(_)) 
  
  /** The unicode directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#unicode-character-codes]] for details.
   */
  lazy val unicode: DirectivePartBuilder[Span] = argument(StandardDirectiveParsers.unicode, withWS = true) map (Text(_))
  
  /** The date directive, 
   *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#date]] for details.
   */
  lazy val date: DirectivePartBuilder[Span] = {
    optArgument(withWS = true) map { pattern => 
      Text(new SimpleDateFormat(pattern.getOrElse("yyyy-MM-dd")).format(new Date))
    } 
  }
  
  /** All standard reStrucuturedText span directives,
   *  to be used in substitution references.
   */
  lazy val spanDirectives: List[Directive[Span]] = List(
    SpanDirective.recursive("image")(StandardDirectiveParts.image),
    SpanDirective("replace")(replace),
    SpanDirective("unicode")(unicode),
    SpanDirective("date")(date)
  )
  
  
}
