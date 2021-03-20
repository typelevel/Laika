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

package laika.parse.css

import laika.ast._
import laika.parse.Parser
import laika.parse.markup.InlineParsers
import laika.parse.builders._
import laika.parse.implicits._

/**
 * Parsers for the subset of CSS supported by Laika.
 * 
 * Not supported are attribute selectors, pseudo classes, namespaces and media queries.
 * 
 * @author Jens Halm
 */
object CSSParsers {

  
  /** Represents a combinator between two predicates.
   */
  sealed abstract class Combinator
  
  /** A combinator for a descendant on any nesting
   *  level.
   */
  case object Descendant extends Combinator
  
  /** A combinator for an immediate child.
   */
  case object Child extends Combinator

  /** Represents a single style within a style
   *  declaration.
   */
  case class Style (name: String, value: String)
  
  /** Parses horizontal whitespace or newline characters.
   */
  val wsOrNl: Parser[String] = anyOf(' ', '\t', '\n', '\r')


  /** Parses the name of a style. The name must start
   *  with a letter, while subsequent characters can be 
   *  letters, digits or one of the symbols `'-'` or `'_'`.
   */
  val styleRefName: Parser[String] = {
    val alpha = someWhile(c => Character.isLetter(c))
    val alphaNum = someWhile(c => Character.isDigit(c) || Character.isLetter(c))
    val symbol = anyOf('-', '_').max(1)
    
    val name = alpha ~ (symbol ~ alphaNum).rep
    (("fox:" ~ name) | name).source
  }
  
  /** Parses a combinator between two predicates.
   */
  val combinator: Parser[Combinator] =
    (ws ~ ">" ~ ws).as(Child) | ws.min(1).as(Descendant)

  /** Parses a single type selector.
   */
  val typeSelector: Parser[List[StylePredicate]] =
    styleRefName.map { name => List(StylePredicate.ElementType(name)) } | literal("*").as(Nil)
    
  /** Parses a single predicate.
   */
  val predicate: Parser[StylePredicate] = {
    
    val id: Parser[StylePredicate]        = "#" ~> styleRefName.map(StylePredicate.Id.apply)
    val styleName: Parser[StylePredicate] = "." ~> styleRefName.map(StylePredicate.StyleName.apply)
    
    id | styleName
  }

  /** Parses the sub-part of a selector without any combinators, e.g. `Paragraph#title`.
    */
  val simpleSelectorSequence: Parser[StyleSelector] =
    ((typeSelector ~ predicate.rep).concat | predicate.rep.min(1)).map(preds => StyleSelector(preds.toSet))

  /** Parses a single selector.
    */
  val selector: Parser[StyleSelector] =
    simpleSelectorSequence ~ (combinator ~ simpleSelectorSequence).rep ^^ {
      case sel ~ sels =>  sels.foldLeft(sel) {
        case (parent, Child ~ sel)      => sel.copy(parent = Some(ParentSelector(parent, immediate = true)))
        case (parent, Descendant ~ sel) => sel.copy(parent = Some(ParentSelector(parent, immediate = false)))
      }
    }

  /** Parses a sequence of selectors, separated by a comma.
    */
  val selectorGroup: Parser[Seq[StyleSelector]] = selector.rep((ws ~ "," ~ ws).void).min(1)

  /** Parses the value of a single style, ignoring
    *  any comments..
    */
  val styleValue: Parser[String] = {
    val comment = ("/*" ~ delimitedBy("*/") ~ wsOrNl).as("")
    InlineParsers.text(delimitedBy(';')).embed(comment)
  }

  /** Parses a single style within a declaration.
    */
  val style: Parser[Style] = ((styleRefName <~ ws ~ ":" ~ ws) ~ (styleValue <~ wsOrNl)).mapN(Style.apply)

  /** Parses a single CSS comment.
    */
  val comment: Parser[Unit] = ("/*" ~ delimitedBy("*/") ~ wsOrNl).void
  
  /** Parses a sequence of style declarations, ignoring
   *  any comments.
   */
  val styleDeclarations: Parser[Seq[StyleDeclaration]] =
    ((selectorGroup <~ wsOrNl ~ "{" ~ wsOrNl) ~ (comment | style).rep <~ (wsOrNl ~ "}"))
      .mapN { (selectors, stylesAndComments) =>
        val styles = stylesAndComments collect { case st: Style => (st.name, st.value) } toMap;
        selectors map (StyleDeclaration(_, styles))
      }
  
  /** Parses an entire set of style declarations.
   *  This is the top level parser of this trait.
   */
  lazy val styleDeclarationSet: Parser[Set[StyleDeclaration]] = {

    (wsOrNl ~ comment.rep ~> (styleDeclarations <~ wsOrNl ~ comment.rep).rep).map {
      _.flatten.zipWithIndex.map({
        case (decl,pos) => decl.increaseOrderBy(pos)
      }).toSet
    }
  }

}
