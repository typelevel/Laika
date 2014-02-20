/*
 * Copyright 2014 the original author or authors.
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

import laika.parse.css.Styles._
import laika.tree.Documents.Path
import laika.tree.Elements.Span
import laika.io.Input
import scala.util.parsing.input.Reader

/**
 * Parsers for the subset of CSS supported by Laika.
 * 
 * Not supported are attribute selectors, pseudo classes, namespaces and media queries.
 * 
 * @author Jens Halm
 */
trait CSSParsers extends laika.parse.InlineParsers { 

  
  sealed abstract class Combinator
  case object Descendant extends Combinator
  case object Child extends Combinator
  
  case class Style (name: String, value: String)
  
  
  val wsOrNl = anyOf(' ','\t', '\n', '\r')
  
  
  lazy val selectorGroup: Parser[Seq[Selector]] = 
    selector ~ ((ws ~ ',' ~ ws ~> selector)*) ^^ { case sel ~ sels => sel :: sels }
  
  lazy val selector: Parser[Selector] = 
    simpleSelectorSequence ~ ((combinator ~ simpleSelectorSequence)*) ^^ { 
      case sel ~ sels => (sel /: sels) {
        case (parent, Child ~ sel)      => sel.copy(parent = Some(ParentSelector(parent, true)))
        case (parent, Descendant ~ sel) => sel.copy(parent = Some(ParentSelector(parent, false)))
      } 
    }
  
  lazy val simpleSelectorSequence: Parser[Selector] =
    (((typeSelector ~ (predicate*)) ^^ { case preds1 ~ preds2 => preds1 ::: preds2 }) | (predicate+)) ^^ {
      preds => Selector(preds.toSet)
  }
  
  lazy val combinator: Parser[Combinator] = 
    ((ws ~ '>' ~ ws) ^^^ Child) | (ws min 1) ^^^ Descendant
  
  lazy val typeSelector: Parser[List[Predicate]] =
    (refName ^^ { name => List(ElementType(name)) }) | ('*' ^^^ Nil) 
    
  lazy val predicate: Parser[Predicate] = id | styleName
  
  lazy val id: Parser[Predicate] = ('#' ~> refName) ^^ Id
  
  lazy val styleName: Parser[Predicate] = ('.' ~> refName) ^^ StyleName
  
  
  lazy val styleDeclarations: Parser[Seq[StyleDeclaration]] = 
    ((selectorGroup <~ wsOrNl ~ '{' ~ wsOrNl) ~ ((comment | style)*) <~ (wsOrNl ~ '}')) ^^ {
      case selectors ~ stylesAndComments => 
        val styles = stylesAndComments collect { case st: Style => (st.name, st.value) } toMap;
        selectors map (StyleDeclaration(_, styles))
    }
  
  lazy val comment: Parser[Unit] = ("/*" ~ anyUntil("*/") ~ wsOrNl) ^^^ ()
    
  lazy val style: Parser[Style] = ((refName <~ ws ~ ':' ~ ws) ~ (styleValue <~ ws ~ ';' ~ wsOrNl)) ^^ {
    case name ~ value => Style(name, value)
  }
  
  lazy val styleValue: Parser[String] = 
    text(anyUntil(';'), Map('/' -> (('*' ~ anyUntil("*/") ~ wsOrNl) ^^^ "")))
    
  lazy val styleDeclarationSet: Parser[Set[StyleDeclaration]] = 
    (wsOrNl ~ (comment*) ~> ((styleDeclarations <~ wsOrNl ~ (comment*))*)) ^^ { 
      _.flatten.zipWithIndex.map({
        case (decl,pos) => decl.increaseOrderBy(pos)
      }).toSet 
    }
  
  def parseStyleSheet (reader: Reader[Char], path: Path) = {
    val set = parseMarkup(styleDeclarationSet, reader)
    new StyleDeclarationSet(path, set)
  }
    
}

object CSSParsers {
  
  lazy val default: Input => StyleDeclarationSet = {
    val parser = new CSSParsers {
      val prepareSpanParsers: Map[Char,Parser[Span]] = Map.empty
    }
    input => parser.parseStyleSheet(input.asParserInput, input.path)
  }
  
}
