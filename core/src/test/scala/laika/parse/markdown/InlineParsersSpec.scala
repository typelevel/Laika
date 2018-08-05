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

package laika.parse.markdown

import laika.api.config.OperationConfig
import laika.format.Markdown
import laika.parse.core.Parser
import laika.parse.core.markup.RootParser
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
     
class InlineParsersSpec extends FlatSpec 
                        with Matchers 
                        with ParseResultHelpers
                        with DefaultParserHelpers[List[Span]] 
                        with ModelBuilder {


  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions).markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.recursiveSpans

  
  "The text parser" should "parse content without any markup as plain text" in {
    Parsing ("some text") should produce (spans(txt("some text")))
  }
  

  
  "The em parser" should "parse content enclosed in * at the beginning of a phrase" in {
    Parsing ("*some* text") should produce (spans(em(txt("some")),txt(" text")))
  }
  
  it should "parse content enclosed in * at the end of a phrase" in {
    Parsing ("some *text*") should produce (spans(txt("some "),em(txt("text"))))
  }
  
  it should "parse content enclosed in * in the middle of a phrase" in {
    Parsing ("some *text* here") should produce (spans(txt("some "),em(txt("text")),txt(" here")))
  }
  
  it should "parse content enclosed in * when it spans the entire phrase" in {
    Parsing ("*text*") should produce (spans(em(txt("text"))))
  }
  
  it should "ignore an * character when it is enclosed in spaces" in {
    Parsing ("some * text * here") should produce (spans(txt("some * text * here")))
  }
  
  it should "ignore an * character when it is not matched by a second *" in {
    Parsing ("some *text here") should produce (spans(txt("some *text here")))
  }
    
  it should "treat an '_' the same as an '*'" in {
    Parsing ("some _text_ here") should produce (spans(txt("some "),em(txt("text")),txt(" here")))
  }
  
  
  
  "The strong parser" should "parse content enclosed in ** at the beginning of a phrase" in {
    Parsing ("**some** text") should produce (spans(str(txt("some")),txt(" text")))
  }
  
  it should "parse content enclosed in ** at the end of a phrase" in {
    Parsing ("some **text**") should produce (spans(txt("some "),str(txt("text"))))
  }
  
  it should "parse content enclosed in ** in the middle of a phrase" in {
    Parsing ("some **text** here") should produce (spans(txt("some "),str(txt("text")),txt(" here")))
  }

  it should "parse content enclosed in ** with a nested em span" in {
    Parsing ("some ***text*** here") should produce (spans(txt("some "),str(em(txt("text"))),txt(" here")))
  }
  
  it should "parse content enclosed in ** when it spans the entire phrase" in {
    Parsing ("**text**") should produce (spans(str(txt("text"))))
  }
  
  it should "ignore a ** sequence when it is enclosed in spaces" in {
    Parsing ("some ** text ** here") should produce (spans(txt("some ** text ** here")))
  }
  
  it should "ignore a ** sequence when it is not matched by a second **" in {
    Parsing ("some **text here") should produce (spans(txt("some **text here")))
  }
  
  it should "treat an '_' the same as an '*'" in {
    Parsing ("some __text__ here") should produce (spans(txt("some "),str(txt("text")),txt(" here")))
  }
  
  
  
  "The code parser" should "parse content enclosed in ` at the beginning of a phrase" in {
    Parsing ("`some` text") should produce (spans(lit("some"),txt(" text")))
  }
  
  it should "parse content enclosed in ` at the end of a phrase" in {
    Parsing ("some `text`") should produce (spans(txt("some "),lit("text")))
  }
  
  it should "parse content enclosed in ` in the middle of a phrase" in {
    Parsing ("some `text` here") should produce (spans(txt("some "),lit("text"),txt(" here")))
  }
  
  it should "parse content enclosed in ` when it spans the entire phrase" in {
    Parsing ("`text`") should produce (spans(lit("text")))
  }
  
  it should "treat a ` character as markup even when it is enclosed in spaces" in {
    Parsing ("some ` text ` here") should produce (spans(txt("some "),lit("text"),txt(" here")))
  }
  
  it should "ignore a ` character when it is not matched by a second `" in {
    Parsing ("some `text here") should produce (spans(txt("some `text here")))
  }
  
  it should "not treat a single ` as markup when the code span is enclosed in double ``" in {
    Parsing ("some ``text`text`` here") should produce (spans(txt("some "),lit("text`text"),txt(" here")))
  }
  
  
  
  "The span parser" should "allow nesting of spans, in this case a code span inside emphasized text" in {
    Parsing ("some *nested `code` span* here") should produce (spans(txt("some "),em(txt("nested "),lit("code"),txt(" span")),txt(" here")))
  }
  
  it should "ignore the attempt to close an outer span inside an inner span" in {
    Parsing ("some *nested `code* span` here") should produce (spans(txt("some *nested "),lit("code* span"),txt(" here")))
  }
  
  
  
  "A backslash " should "cause a following escapable character not to be treated as markup" in {
    Parsing ("""some \*text* here""") should produce (spans(txt("some *text* here")))
  }
  
  it should "be treated as a literal character when the following character is not escapable" in {
    Parsing ("""some \?text here""") should produce (spans(txt("""some \?text here""")))
  }
  
  
  
  "The link parser" should "parse an inline link without title" in {
    Parsing ("some [link](http://foo) here") should produce {
      spans(txt("some "), link(txt("link")).url("http://foo"), txt(" here"))
    }
  }
  
  it should "parse an inline link with an optional title enclosed in double quotes" in {
    Parsing ("""some [link](http://foo "a title") here""") should produce {
      spans(txt("some "), link(txt("link")).url("http://foo").title("a title"), txt(" here"))
    }
  }
  
  it should "parse an inline link with an optional title enclosed in single quotes" in {
    Parsing ("""some [link](http://foo 'a title') here""") should produce (
        spans(txt("some "), link(txt("link")).url("http://foo").title("a title"), txt(" here")))
  }
  
  it should "ignore an inline link with a malformed title" in {
    Parsing ("""some [link](http://foo 'a title) here""") should produce {
      spans(txt("some "), linkRef(txt("link")).id("link").source("[link]"), txt("(http://foo 'a title) here"))
    }
  }
  
  it should "parse markup inside the text of an inline link" in {
    Parsing ("some [link *text*](http://foo) here") should produce {
      spans(txt("some "), link(txt("link "),em(txt("text"))).url("http://foo"), txt(" here"))
    }
  }

  it should "properly parse escape sequences in the text of an inline link" in {
    Parsing ("some [link \\_text\\_](http://foo) here") should produce {
      spans(txt("some "), link(txt("link _text_")).url("http://foo"), txt(" here"))
    }
  }
  
  
  
  "The image parser" should "parse an inline image without title" in {
    Parsing ("some ![link](http://foo.jpg) here") should produce {
      spans(txt("some "), img("link","http://foo.jpg"), txt(" here"))
    }
  }
  
  it should "parse an inline image with an optional title enclosed in double quotes" in {
    Parsing ("""some ![link](http://foo.jpg "a title") here""") should produce {
      spans(txt("some "), img("link","http://foo.jpg",title = Some("a title")), txt(" here"))
    }
  }
  
  it should "parse an inline image with an optional title enclosed in single quotes" in {
    Parsing ("""some ![link](http://foo.jpg 'a title') here""") should produce {
      spans(txt("some "), img("link","http://foo.jpg",title = Some("a title")), txt(" here"))
    }
  }
  
  it should "ignore an inline image with a malformed title" in {
    Parsing ("""some ![link](http://foo.jpg 'a title) here""") should produce {
      spans(txt("some "), imgRef("link","link","![link]"), txt("(http://foo.jpg 'a title) here"))
    }
  }
  
  it should "not parse markup inside the text of an inline link" in {
    Parsing ("some ![link *text*](http://foo.jpg) here") should produce {
      spans(txt("some "), img("link *text*","http://foo.jpg"), txt(" here"))
    }
  }
  
  
  
  "The link reference parser" should "parse a link reference with an explicit id" in {
    Parsing ("some [link][id] here") should produce {
      spans(txt("some "), linkRef(txt("link")).id("id").source("[link][id]"), txt(" here"))
    }
  }
  
  it should "parse a link reference with an empty id" in {
    Parsing ("some [link][] here") should produce {
      spans(txt("some "), linkRef(txt("link")).id("link").source("[link][]"), txt(" here"))
    }
  }
  
  it should "parse a link reference with an explicit id separated by a space" in {
    Parsing ("some [link] [id] here") should produce {
      spans(txt("some "), linkRef(txt("link")).id("id").source("[link] [id]"), txt(" here"))
    }
  }
  
  it should "parse a link reference with an empty id separated by a space" in {
    Parsing ("some [link] [] here") should produce {
      spans(txt("some "), linkRef(txt("link")).id("link").source("[link] []"), txt(" here"))
    }
  }
  
  it should "parse a link reference with an implicit id" in {
    Parsing ("some [link] here") should produce {
      spans(txt("some "), linkRef(txt("link")).id("link").source("[link]"), txt(" here"))
    }
  }
  
  
  
  "The image reference parser" should "parse an image reference with an explicit id" in {
    Parsing ("some ![image][id] here") should produce {
      spans(txt("some "), imgRef("image","id","![image][id]"), txt(" here"))
    }
  }
  
  it should "parse an image reference with an empty id" in {
    Parsing ("some ![image][] here") should produce {
      spans(txt("some "), imgRef("image","image","![image][]"), txt(" here"))
    }
  }
  
  it should "parse an image reference with an implicit id" in {
    Parsing ("some ![image] here") should produce {
      spans(txt("some "), imgRef("image","image","![image]"), txt(" here"))
    }
  }
  
  
  
  "The simple link parser" should "parse a link enclosed in angle brackets and set the url as the link text" in {
    Parsing ("some <http://foo> here") should produce {
      spans(txt("some "), link(txt("http://foo")).url("http://foo"), txt(" here"))
    }
  }
  
  
}
