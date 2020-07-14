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

package laika.parse.code

/** Represents a category that can be assigned to a span inside a code block
  * to facilitate syntax highlighting.
  * 
  * @author Jens Halm
  */
sealed trait CodeCategory extends Product with Serializable {
  protected def prefix: String = ""
  private def camel2dash(text: String) = text.drop(1).foldLeft(text.head.toLower.toString) {
    case (acc, c) if c.isUpper => acc + "-" + c.toLower
    case (acc, c) => acc + c
  }
  /** The name of the category which usually translates to styles in rendered output. */
  def name: String = prefix + camel2dash(productPrefix)
}

object CodeCategory {

  case object Comment extends CodeCategory
  case object Annotation extends CodeCategory
  case object Keyword extends CodeCategory
  case object BooleanLiteral extends CodeCategory
  case object NumberLiteral extends CodeCategory
  case object StringLiteral extends CodeCategory
  case object CharLiteral extends CodeCategory
  case object SymbolLiteral extends CodeCategory
  case object RegexLiteral extends CodeCategory
  case object LiteralValue extends CodeCategory
  case object EscapeSequence extends CodeCategory
  case object Substitution extends CodeCategory
  case object TypeName extends CodeCategory
  case object AttributeName extends CodeCategory
  case object DeclarationName extends CodeCategory
  case object Identifier extends CodeCategory

  /** Common categories for tag-based syntax such as HTML or XML. */
  object Tag {
    sealed trait TagCategory extends CodeCategory {
      override def prefix: String = "tag-"
    }
    case object Name extends TagCategory
    case object Punctuation extends TagCategory
  }

  /** Categories specific to the XML format. */
  object XML {
    sealed trait XMLCategory extends CodeCategory {
      override def prefix: String = "xml-"
    }
    case object DTDTagName extends XMLCategory {
      override def name: String = "xml-dtd-tag-name"
    }
    case object ProcessingInstruction extends XMLCategory
    case object CData extends XMLCategory {
      override def name: String = "xml-cdata"
    }
  }

  /** Categories for text markup formats. */
  object Markup {
    sealed trait MarkupCategory extends CodeCategory {
      override def prefix: String = "markup-"
    }
    case object Fence extends MarkupCategory
    case object Headline extends MarkupCategory
    case object Emphasized extends MarkupCategory
    case object Quote extends MarkupCategory
    case object LinkText extends MarkupCategory
    case object LinkTarget extends MarkupCategory
  }
}
