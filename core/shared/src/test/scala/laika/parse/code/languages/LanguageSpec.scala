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

package laika.parse.code.languages

import laika.api.MarkupParser
import laika.ast._
import laika.config.LaikaKeys
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.code.{CodeCategory, SyntaxHighlighting}
import laika.parse.code.CodeCategory._
import laika.parse.markup.DocumentParser.ParserError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class LanguageSpec extends AnyWordSpec with Matchers {

  
  "The syntax highlighter for code blocks" should {
    
    def parse (input: String): Either[ParserError, RootElement] = MarkupParser
      .of(Markdown)
      .using(GitHubFlavor, SyntaxHighlighting)
      .withConfigValue(LaikaKeys.firstHeaderAsTitle, true)
      .build
      .parse(input)
      .map(_.content)
    
    val space: CodeSpan = CodeSpan(" ")
    val colonSpace: CodeSpan = CodeSpan(": ")
    val comma: CodeSpan = CodeSpan(", ")
    val equals: CodeSpan = CodeSpan(" = ")
    val dot: CodeSpan = CodeSpan(".")
    
    def annotation(value: String): CodeSpan = CodeSpan(s"@$value", Annotation)
    def keyword(value: String): CodeSpan = CodeSpan(value, Keyword)
    def id(value: String): CodeSpan = CodeSpan(value, Identifier)
    def typeName(value: String): CodeSpan = CodeSpan(value, TypeName)
    def boolean(value: String): CodeSpan = CodeSpan(value, BooleanLiteral)
    def string(value: String): CodeSpan = CodeSpan(value, StringLiteral)
    def number(value: String): CodeSpan = CodeSpan(value, NumberLiteral)
    def char(value: String): CodeSpan = CodeSpan(value, CharLiteral)
    def escape(value: String): CodeSpan = CodeSpan(value, EscapeSequence)
    def subst(value: String): CodeSpan = CodeSpan(value, Substitution)
    def literal(value: String): CodeSpan = CodeSpan(value, LiteralValue)
    def tagName(value: String): CodeSpan = CodeSpan(value, CodeCategory.Tag.Name)
    def attrName(value: String): CodeSpan = CodeSpan(value, CodeCategory.AttributeName)
    def declName(value: String): CodeSpan = CodeSpan(value, CodeCategory.DeclarationName)
    def comment(value: String): CodeSpan = CodeSpan(value, CodeCategory.Comment)
    def other(value: String): CodeSpan = CodeSpan(value)
    def multiline(value: String): CodeSpan = string("\"\"\""+value+"\"\"\"")
    
    def result(lang: String, spans: CodeSpan*): Either[ParserError, RootElement] = Right(RootElement(
      Title(Seq(Text("Doc")), Style.title + Id("doc")),
      CodeBlock(lang, spans.toSeq)
    ))
    
    trait TagFormats {
      def string(value: String): CodeSpan = CodeSpan("\"" + value + "\"", StringLiteral)
      def qstring(value: String): CodeSpan = CodeSpan("\"" + value + "\"", StringLiteral)
      def dtdTag(value: String): CodeSpan = CodeSpan(value, CodeCategory.XML.DTDTagName)
      def nl(indent: Int): CodeSpan = CodeSpan("\n" + (" " * indent))
      def punct(content: String): CodeSpan = CodeSpan(content, CodeCategory.Tag.Punctuation)
      val open: CodeSpan = CodeSpan("<", CodeCategory.Tag.Punctuation)
      val close: CodeSpan = CodeSpan(">", CodeCategory.Tag.Punctuation)
      val spacePunct: CodeSpan = CodeSpan(" ", CodeCategory.Tag.Punctuation)
      val eq: CodeSpan = CodeSpan("=", CodeCategory.Tag.Punctuation)
    }
    
    trait MarkupFormats {
      def txt(content: String): CodeSpan = CodeSpan(content)
      def header(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Headline)
      def em(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Emphasized)
      def linkText(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.LinkText)
      def linkTarget(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.LinkTarget)
    }
    
    "parse Scala code" in {
      
      val input =
        """#Doc
          |
          |```scala
          |@Thing case class Foo (bar: Int, baz: String) {
          |
          |  val xx = "some \t value"
          |  
          |  lazy val `y-y` = +++line 1
          |    |line 2+++.stripMargin
          |  
          |  def bag = Seq(true, null, 's', 0xff, s"one $subst, two ${subst.get}")
          |  
          |  // just a short example
          |  
          |}
          |```
        """.stripMargin.replace("+++", "\"\"\"")
      
      parse(input) shouldBe result("scala",
        annotation("Thing"), space, keyword("case"), space, keyword("class"), space, typeName("Foo"), 
        other(" ("), id("bar"), colonSpace, typeName("Int"), comma, id("baz"), colonSpace, typeName("String"), other(") {\n\n  "),
        keyword("val"), space, id("xx"), equals, string("\"some "), escape("\\t"), string(" value\""), other("\n  \n  "),
        keyword("lazy"), space, keyword("val"), space, id("`y-y`"), equals,
        string("\"\"\"line 1\n    |line 2\"\"\""), dot, id("stripMargin"), other("\n  \n  "),
        keyword("def"), space, declName("bag"), equals, typeName("Seq"), other("("), 
        boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), comma,
        string("s\"one "), subst("$subst"), string(", two "), subst("${subst.get}"), string("\""), other(")\n  \n  "),
        comment("// just a short example\n"),
        other("  \n}")
      )
      
    }

    "parse Dotty code" in {

      val input =
        """#Doc
          |
          |```dotty
          |@Thing case class Foo (bar: Int, baz: String) {
          |
          |  opaque type Xyz = Int
          |
          |  given global as ExecutionContext = new ForkJoinPool()
          |
          |  inline val xx = "some \t value"
          |  
          |  lazy val `y-y` = +++line 1
          |    |line 2+++.stripMargin
          |  
          |  def bag(using ExecutionContext) = Seq(true, null, 's', 0xff)
          |  
          |  // just a short example
          |  
          |}
          |```
        """.stripMargin.replace("+++", "\"\"\"")

      parse(input) shouldBe result("dotty",
        annotation("Thing"), space, keyword("case"), space, keyword("class"), space, typeName("Foo"),
        other(" ("), id("bar"), colonSpace, typeName("Int"), comma, id("baz"), colonSpace, typeName("String"), other(") {\n\n  "),
        keyword("opaque"), space, keyword("type"), space, typeName("Xyz"), equals, typeName("Int"), other("\n\n  "),
        keyword("given"), space, id("global"), space, keyword("as"), space, typeName("ExecutionContext"), equals, 
        keyword("new"), space, typeName("ForkJoinPool"), other("()\n\n  "),
        keyword("inline"), space, keyword("val"), space, id("xx"), equals, string("\"some "), escape("\\t"), string(" value\""), other("\n  \n  "),
        keyword("lazy"), space, keyword("val"), space, id("`y-y`"), equals,
        string("\"\"\"line 1\n    |line 2\"\"\""), dot, id("stripMargin"), other("\n  \n  "),
        keyword("def"), space, declName("bag"), other("("), keyword("using"), space, typeName("ExecutionContext"), 
        other(") = "), typeName("Seq"), other("("),
        boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), other(")\n  \n  "),
        comment("// just a short example\n"),
        other("  \n}")
      )

    }

    "parse Java code" in {

      val input =
        """#Doc
          |
          |```java
          |@Thing class Foo {
          |
          |  private List<Object> xx = new List<Object>(true, null, 's', 0xff, "some \t value")
          |  
          |  int calc (int bar, String baz) { return bar; }
          |  
          |  // just a short example
          |  
          |}
          |```
        """.stripMargin.replace("+++", "\"\"\"")

      parse(input) shouldBe result("java",
        annotation("Thing"), space, keyword("class"), space, typeName("Foo"), other(" {\n\n  "),
        keyword("private"), space, typeName("List"), other("<"), typeName("Object"), other("> "), id("xx"), equals,
        keyword("new"), space, typeName("List"), other("<"), typeName("Object"), other(">("),
        boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), comma, 
        string("\"some "), escape("\\t"), string(" value\""), other(")\n  \n  "),
        typeName("int"), space, id("calc"), other(" ("), typeName("int"), space, id("bar"), comma, typeName("String"), space, id("baz"), other(") { "),
        keyword("return"), space, id("bar"), other("; }\n  \n  "),
        comment("// just a short example\n"),
        other("  \n}")
      )

    }

    "parse Python code" in {

      val input =
        """#Doc
          |
          |```python
          |import re
          |for test_string in ['555-1212', 'ILL-EGAL']:
          |    if re.match(r'^\d{3}-\d{4}$', test_string):
          |        print (test_string, 'is a valid US local phone number')
          |    else:
          |        print (test_string, 'rejected')
          |```
        """.stripMargin.replace("+++", "\"\"\"")

      parse(input) shouldBe result("python",
        keyword("import"), space, id("re"), other("\n"), 
        keyword("for"), space, id("test_string"), space, keyword("in"), other(" ["), string("'555-1212'"), comma, string("'ILL-EGAL'"), other("]:\n    "),
        keyword("if"), space, id("re"), dot, id("match"), other("("), string("r'^\\d{3}-\\d{4}$'"), comma, id("test_string"), other("):\n        "),
        keyword("print"), other(" ("), id("test_string"), comma, string("'is a valid US local phone number'"), other(")\n    "),
        keyword("else"), other(":\n        "), keyword("print"), other(" ("), id("test_string"), comma, string("'rejected'"), other(")")
      )

    }
    
    "parse JavaScript code" in {
      val input = 
      """#Doc
        |
        |```js
        |class App extends Component {
        |
        |  state = {
        |    lastResult: this.message('Apocalypse started')
        |  }
        |
        |  handleError = error => {
        |    console.log(error);
        |    const msg = (error.response) ? `Status: ${error.response.status}` : 'Unknown error';
        |    this.setState({ lastResult: this.message(`Server Error (${msg})`) });
        |  }
        |}
        |```""".stripMargin

      parse(input) shouldBe result("js",
        keyword("class"), space, id("App"), space, keyword("extends"), space, id("Component"), other(" {\n\n  "),
        id("state"), other(" = {\n    "),
        id("lastResult"), colonSpace, keyword("this"), dot, id("message"), other("("), string("'Apocalypse started'"), other(")\n  }\n\n  "),
        id("handleError"), equals, id("error"), other(" => {\n    "),
        id("console"), dot, id("log"), other("("), id("error"), other(");\n    "),
        keyword("const"), space, id("msg"), other(" = ("), id("error"), dot, id("response"), other(") ? "),
        string("`Status: "), subst("${error.response.status}"), string("`"), other(" : "), string("'Unknown error'"), other(";\n    "),
        keyword("this"), dot, id("setState"), other("({ "), id("lastResult"), colonSpace, 
        keyword("this"), dot, id("message"), other("("), string("`Server Error ("), subst("${msg}"), string(")`"), 
        other(") });\n  }\n}")
      )
    }

    "parse JSX code" in {
      val input =
        """#Doc
          |
          |```jsx
          |class App extends Component {
          |
          |  state = {
          |    lastResult: this.message('Apocalypse started')
          |  }
          |
          |  tag = <div className="big"><Comp.Hello>See {props.foo}</Comp.Hello></div>
          |
          |  handleError = error => {
          |    console.log(error);
          |    const msg = (error.response) ? `Status: ${error.response.status}` : 'Unknown error';
          |    this.setState({ lastResult: this.message(`Server Error (${msg})`) });
          |  }
          |}
          |```""".stripMargin

      val tags = new TagFormats {}
      parse(input) shouldBe result("jsx",
        keyword("class"), space, id("App"), space, keyword("extends"), space, id("Component"), other(" {\n\n  "),
        id("state"), other(" = {\n    "),
        id("lastResult"), colonSpace, keyword("this"), dot, id("message"), other("("), string("'Apocalypse started'"), other(")\n  }\n\n  "),
        id("tag"), equals, tags.punct("<"), tagName("div"), tags.spacePunct, attrName("className"), tags.eq,
        string("\"big\""), tags.punct("><"), typeName("Comp.Hello"), tags.punct(">"), other("See "), subst("{props.foo}"),
        tags.punct("</"), typeName("Comp.Hello"), tags.punct("></"), tagName("div"), tags.punct(">"), other("\n\n  "),
        id("handleError"), equals, id("error"), other(" => {\n    "),
        id("console"), dot, id("log"), other("("), id("error"), other(");\n    "),
        keyword("const"), space, id("msg"), other(" = ("), id("error"), dot, id("response"), other(") ? "),
        string("`Status: "), subst("${error.response.status}"), string("`"), other(" : "), string("'Unknown error'"), other(";\n    "),
        keyword("this"), dot, id("setState"), other("({ "), id("lastResult"), colonSpace,
        keyword("this"), dot, id("message"), other("("), string("`Server Error ("), subst("${msg}"), string(")`"),
        other(") });\n  }\n}")
      )
    }

    "parse Haskell code" in {
      val input =
        """#Doc
          |
          |```hs
          |module Main where
          |
          |-- comment
          |import qualified Data.Map as M -- some comment
          |
          |errorsPerLine = M.fromList
          |    [ ("Chr\"is module", 472), ("Don import", 100), ("Simon case", -5) ]
          |
          |aChar = 'b'
          |
          |changePrice :: Thing -> Price -> Thing
          |changePrice x new = x { price = new }
          |
          |{- multiline
          |   comment
          |-}
          |main = do putStrLn "Who are you?"
          |          name <- getLine
          |          case M.lookup name errorsPerLine of
          |              Nothing -> putStrLn "I don't know you"
          |              Just n  -> do putStr "Errors per line: "
          |                            print n
          |```
          |""".stripMargin


      val nl = other("\n")
      val arrow = other(" -> ")
      val leftArrow = other(" <- ")
      parse(input) shouldBe result("hs",
        keyword("module"), space, typeName("Main"), space, keyword("where"), other("\n\n"),
        comment("-- comment\n"), keyword("import"), space, keyword("qualified"), space, typeName("Data"), dot, typeName("Map"), space, keyword("as"), space, typeName("M"), space, comment("-- some comment\n"), nl,
        id("errorsPerLine"), other(" = "), typeName("M"), dot, id("fromList"),
        other("\n    [ ("), string("\"Chr"), escape("\\\""), string("is module\""), comma, number("472"), other("), ("), string("\"Don import\""), comma, number("100"), other("), ("), string("\"Simon case\""), other(", -"), number("5"), other(") ]\n\n"),
        id("aChar"), other(" = "), char("'b'"), other("\n\n"),
        id("changePrice"), other(" :: "), typeName("Thing"), arrow, typeName("Price"), arrow, typeName("Thing"), nl,
        id("changePrice"), space, id("x"), space, id("new"), other(" = "), id("x"), other(" { "), id("price"), other(" = "), id("new"), other(" }\n\n"),
        comment("{- multiline\n   comment\n-}"), nl,
        id("main"), other(" = "), keyword("do"), space, id("putStrLn"), space, string("\"Who are you?\""), other("\n          "),
        id("name"), leftArrow, id("getLine"), other("\n          "),
        keyword("case"), space, typeName("M"), other("."), id("lookup"), space, id("name"), space, id("errorsPerLine"), space, keyword("of"), other("\n              "),
        typeName("Nothing"), arrow, id("putStrLn"), space, string("\"I don't know you\""), other("\n              "),
        typeName("Just"), space, id("n"), other("  -> "), keyword("do"), space, id("putStr"), space, string("\"Errors per line: \""), other("\n                            "),
        id("print"), space, id("n")
      )
    }
    
    "parse TypeScript code" in {
      val input =
      """#Doc
        |
        |```typescript
        |import * as React from "react";
        |
        |interface Props {
        |  initialUserName: string;
        |  onNameUpdated: (newName: string) => any;
        |}
        |
        |export const NameEditComponent = (props: Props) => {
        |
        |  const [editingName, setEditingName] = React.useState(props.initialUserName);
        |
        |  const onChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        |    setEditingName(e.target.value);
        |  };
        |
        |}
        |```
        """.stripMargin

      parse(input) shouldBe result("typescript",
        keyword("import"), other(" * "), keyword("as"), space, id("React"), space, keyword("from"), space, string("\"react\""), other(";\n\n"),
        keyword("interface"), space, id("Props"), other(" {\n  "),
        id("initialUserName"), colonSpace, typeName("string"), other(";\n  "),
        id("onNameUpdated"), other(": ("), id("newName"), colonSpace, typeName("string"), other(") => "), typeName("any"), other(";\n}\n\n"),
        keyword("export"), space, keyword("const"), space, id("NameEditComponent"), other(" = ("), id("props"), colonSpace, id("Props"), other(") => {\n\n  "),
        keyword("const"), other(" ["), id("editingName"), comma, id("setEditingName"), other("] = "), 
        id("React"), dot, id("useState"), other("("), id("props"), dot, id("initialUserName"), other(");\n\n  "),
        keyword("const"), space, id("onChange"), other(" = ("), id("e"), colonSpace, id("React"), dot, id("ChangeEvent"), other("<"), id("HTMLInputElement"), other(">) => {\n    "),
        id("setEditingName"), other("("), id("e"), dot, id("target"), dot, id("value"), other(");\n  };\n\n}")
      )
    }

    "parse TSX code" in {
      val input =
        """#Doc
          |
          |```tsx
          |import * as React from "react";
          |
          |interface Props {
          |  initialUserName: string;
          |  onNameUpdated: (newName: string) => any;
          |}
          |
          |export const NameEditComponent = (props: Props) => {
          |
          |  const [editingName, setEditingName] = React.useState(props.initialUserName);
          |
          |  const tag = <div className="big"><Comp.Hello>See {props.foo}</Comp.Hello></div>;
          |
          |  const onChange = (e: React.ChangeEvent<HTMLInputElement>) => {
          |    setEditingName(e.target.value);
          |  };
          |
          |}
          |```
        """.stripMargin

      val tags = new TagFormats {}
      parse(input) shouldBe result("tsx",
        keyword("import"), other(" * "), keyword("as"), space, id("React"), space, keyword("from"), space, string("\"react\""), other(";\n\n"),
        keyword("interface"), space, id("Props"), other(" {\n  "),
        id("initialUserName"), colonSpace, typeName("string"), other(";\n  "),
        id("onNameUpdated"), other(": ("), id("newName"), colonSpace, typeName("string"), other(") => "), typeName("any"), other(";\n}\n\n"),
        keyword("export"), space, keyword("const"), space, id("NameEditComponent"), other(" = ("), id("props"), colonSpace, id("Props"), other(") => {\n\n  "),
        keyword("const"), other(" ["), id("editingName"), comma, id("setEditingName"), other("] = "),
        id("React"), dot, id("useState"), other("("), id("props"), dot, id("initialUserName"), other(");\n\n  "),
        keyword("const"), space, id("tag"), equals, tags.punct("<"), tagName("div"), tags.spacePunct, attrName("className"), tags.eq,
        string("\"big\""), tags.punct("><"), typeName("Comp.Hello"), tags.punct(">"), other("See "), subst("{props.foo}"),
        tags.punct("</"), typeName("Comp.Hello"), tags.punct("></"), tagName("div"), tags.punct(">"), other(";\n\n  "),
        keyword("const"), space, id("onChange"), other(" = ("), id("e"), colonSpace, id("React"), dot, id("ChangeEvent"), other("<"), id("HTMLInputElement"), other(">) => {\n    "),
        id("setEditingName"), other("("), id("e"), dot, id("target"), dot, id("value"), other(");\n  };\n\n}")
      )
    }

    "parse Alloy code" in {
      val input =
        """# Doc
          |
          |```alloy
          |open util/ordering[House]
          |
          |enum Color {Red, White, Blue, Green, Yellow} // some comment
          |sig House {
          |        color: disj Color
          |}
          |/* Another
          |   comment */
          |abstract sig Owner {
          |        house: disj House
          |}
          |one sig Brit extends Owner {}
          |fact constraints {
          |        one b: Brit | b.house.color = Red
          |        #House = 5
          |}
          |run {} for 5
          |```
          |""".stripMargin

      val dot = other(".")
      parse(input) shouldBe result("alloy",
        keyword("open"), space, id("util"), other("/"), id("ordering"), other("["), id("House"), other("]\n\n"),
        keyword("enum"), space, id("Color"), other(" {"), id("Red"), other(", "), id("White"), other(", "), id("Blue"), other(", "), id("Green"), other(", "), id("Yellow"), other("} "), comment("// some comment\n"),
        keyword("sig"), space, id("House"), other(" {\n        "), id("color"), colonSpace, keyword("disj"), space, id("Color"), other("\n}\n"),
        comment("/* Another\n   comment */"), other("\n"),
        keyword("abstract"), space, keyword("sig"), space, id("Owner"), other(" {\n        "), id("house"), colonSpace, keyword("disj"), space, id("House"), other("\n}\n"),
        keyword("one"), space, keyword("sig"), space, id("Brit"), space, keyword("extends"), space, id("Owner"), other(" {}\n"),
        keyword("fact"), space, id("constraints"), other(" {\n        "),
        keyword("one"), space, id("b"), colonSpace, id("Brit"), other(" | "), id("b"), dot, id("house"), dot, id("color"), other(" = "), id("Red"), other("\n        #"),
        id("House"), other(" = "), number("5"), other("\n}\n"),
        keyword("run"), other(" {} "), keyword("for"), space, number("5")
      )
    }
    
    "parse an XML document" in new TagFormats {
      val input =
        """# Doc
          |
          |```xml
          |<?xml version="1.0"?>
          |<!DOCTYPE foo [
          |  <!ELEMENT bar (baz)>
          |  <!ELEMENT baz (#PCDATA)>
          |  <!ATTLIST bar bar_no CDATA #REQUIRED>
          |  <!ENTITY logo PUBLIC  "-//W3C//GIF logo//EN" "http://www.w3.org/logo.gif" NDATA gif>
          |]>
          |<foo>
          |  <bar bar_no="xyz-123">
          |    <baz>Some text with &lt; entities &#x20;</baz>
          |  </bar>
          |  <!-- some comment -->
          |  <? some pi ?>
          |  <![CDATA[some cdata content]]>
          |</foo>
          |```
        """.stripMargin

      

      parse(input) shouldBe result("xml",
        punct("<?"), tagName("xml"), spacePunct, attrName("version"), eq, qstring("1.0"), punct("?>"), nl(0),
        punct("<!"), dtdTag("DOCTYPE"), spacePunct, id("foo"), punct(" [\n  <!"),
        dtdTag("ELEMENT"), spacePunct, id("bar"), punct(" ("), id("baz"), punct(")>\n  <!"),
        dtdTag("ELEMENT"), spacePunct, id("baz"), punct(" ("), keyword("#PCDATA"), punct(")>\n  <!"),
        dtdTag("ATTLIST"), spacePunct, id("bar"), spacePunct, id("bar_no"), spacePunct, keyword("CDATA"), spacePunct, keyword("#REQUIRED"), punct(">\n  <!"),
        dtdTag("ENTITY"), spacePunct, id("logo"), spacePunct, keyword("PUBLIC"), punct("  "), qstring("-//W3C//GIF logo//EN"), spacePunct,
        qstring("http://www.w3.org/logo.gif"), spacePunct, keyword("NDATA"), spacePunct, id("gif"), punct(">\n]>"), nl(0),
        open, tagName("foo"), close, nl(2),
        open, tagName("bar"), spacePunct, attrName("bar_no"), eq, qstring("xyz-123"), close, nl(4),
        open, tagName("baz"), close, other("Some text with "), subst("&lt;"), other(" entities "), escape("&#x20;"), punct("</"), tagName("baz"), close, nl(2),
        punct("</"), tagName("bar"), close, nl(2),
        comment("<!-- some comment -->"), nl(2),
        CodeSpan("<? some pi ?>", CodeCategory.XML.ProcessingInstruction), nl(2),
        CodeSpan("<![CDATA[some cdata content]]>", CodeCategory.XML.CData), nl(0),
        punct("</"), tagName("foo"), close
      )
    }

    "parse an HTML document" in new TagFormats {
      val input =
        """# Doc
          |
          |```html
          |<!DOCTYPE html>
          |<html>
          |  <body>
          |    <style>
          |      p {color:blue;}
          |    </style>
          |    <script>
          |      var x = "foo";
          |      var y = 97.5;
          |    </script>
          |    <script src="foo.js"/>
          |    <p class="big">Some text with &lt; entities &#x20;</p>
          |    <!-- some comment -->
          |  </body>
          |</html>
          |``` 
        """.stripMargin

      parse(input) shouldBe result("html",
        punct("<!"), dtdTag("DOCTYPE"), spacePunct, id("html"), close, nl(0),
        open, tagName("html"), close, nl(2),
        open, tagName("body"), close, nl(4),
        open, tagName("style"), close, nl(6),
        id("p"), other(" {"), attrName("color"), other(":"), id("blue"), other(";}\n    "),
        punct("</"), tagName("style"), close, nl(4),
        open, tagName("script"), close, nl(6),
        keyword("var"), other(" "), id("x"), other(" = "), qstring("foo"), other(";\n      "),
        keyword("var"), other(" "), id("y"), other(" = "), number("97.5"), other(";\n    "),
        punct("</"), tagName("script"), close, nl(4),
        open, tagName("script"), spacePunct, attrName("src"), eq, qstring("foo.js"), punct("/>"), nl(4),
        open, tagName("p"), spacePunct, attrName("class"), eq, qstring("big"), close, other("Some text with "),
        subst("&lt;"), other(" entities "), escape("&#x20;"), punct("</"), tagName("p"), close, nl(4),
        comment("<!-- some comment -->"), nl(2),
        punct("</"), tagName("body"), close, nl(0),
        punct("</"), tagName("html"), close
      )
    }

    "parse a CSS document" in {
      val input =
        """# Doc
          |
          |```css
          |:first {
          |  color: #3f51b5
          |}
          |p {
          |  font-family: Monaco, "Courier New", monospace;
          |  font-size: 1.5rem
          |}
          |.foo {
          |  /* comment */
          |  -custom: url(http://foo.bar/);
          |  font-family: "Weird \\ Font", sans-serif
          |}
          |#bar {
          |  border: 1px solid rgba(0, 0, 0, .06)
          |}
          |@media (max-width: 991.98px) {
          |  font-size: 1rem
          |}
          |``` 
        """.stripMargin
      
      val openDecl = other(" {\n  ")
      val closeDecl = other("\n}\n")
      val sep = other(";\n  ")

      parse(input) shouldBe result("css",
        other(":"), id("first"), openDecl,
        attrName("color"), colonSpace, number("#3f51b5"), closeDecl,
        id("p"), openDecl,
        attrName("font-family"), colonSpace, id("Monaco"), comma, string("\"Courier New\""), comma, id("monospace"), sep,
        attrName("font-size"), colonSpace, number("1.5"), id("rem"), other("\n}\n."),
        id("foo"), openDecl,
        comment("/* comment */"), other("\n  "),
        attrName("-custom"), colonSpace, id("url"), other("("), string("http://foo.bar/"), other(");\n  "),
        attrName("font-family"), colonSpace, string("\"Weird "), escape("\\\\"), string(" Font\""), comma, id("sans-serif"), closeDecl,
        id("#bar"), openDecl,
        attrName("border"), colonSpace, number("1"), id("px"), space, id("solid"), space, id("rgba"), other("("),
        number("0"), comma, number("0"), comma, number("0"), comma, number(".06"), other(")\n}\n"),
        id("@media"), other(" ("), attrName("max-width"), colonSpace, number("991.98"), id("px"), other(") {\n  "),
        attrName("font-size"), colonSpace, number("1"), id("rem"), other("\n}")
      )
    }

    "parse a JSON document" in {
      val input =
        """# Doc
          |
          |```json
          |{
          |  "foo": [false, 2.3e-2, null, "bar", { "nested": true }
          |}
          |``` 
        """.stripMargin

      def attrName(value: String): CodeSpan = CodeSpan("\"" + value + "\"", CodeCategory.AttributeName)

      parse(input) shouldBe result("json",
        other("{\n  "),
        attrName("foo"), other(": ["), boolean("false"), comma, number("2.3e-2"), comma, literal("null"), comma, string("\"bar\""),
        other(", { "), attrName("nested"), colonSpace, boolean("true"), other(" }\n}")
      )
    }

    "parse a HOCON document" in {
      val input =
        """# Doc
          |
          |```hocon
          |{
          |  a = [false, 2.3e-2, null, "bar", { nested = true }
          |  
          |  include required(file("xx.conf"))
          |  
          |  b = unquoted string # comment
          |  
          |  "c" : text ${subst} text
          |  
          |  d e f = +++multiline
          |          string+++
          |}
          |``` 
        """.stripMargin.replace("+++","\"\"\"")

      def nl(end: String): CodeSpan = CodeSpan(end + "\n  \n  ")
      
      parse(input) shouldBe result("hocon",
        other("{\n  "),
        attrName("a"), other(" = ["), boolean("false"), comma, number("2.3e-2"), comma, literal("null"), comma, string("\"bar\""),
        other(", { "), attrName("nested"), equals, boolean("true"), nl(" }"),
        keyword("include"), space, id("required"), other("("), id("file"), other("("), string("\"xx.conf\""), nl("))"),
        attrName("b"), equals, string("unquoted string"), space, comment("# comment\n"), other("  \n  "),
        attrName("\"c\""), other(" : "), string("text"), space, subst("${subst}"), space, string("text"), nl(""),
        attrName("d e f"), equals, multiline("multiline\n          string"), other("\n}"),
      )
    }

    "parse a Dhall document" in {
      val input =
        """# Doc
          |
          |```dhall
          |-- Single-line comment
          |let txt = "some \t text"
          |let multilineText =
          |  ''
          |  Multi-line
          |  ${txt}
          |  text
          |  ''
          |let bool = True
          |let renderedBool : Text = if bool then "True" else "False"
          |let naturalNumber : Natural = 42
          |let positiveInteger : Integer = +55
          |let negativeInteger : Integer = -12
          |let pi : Double = 3.14159265359
          |let untyped = 123
          |let AttributeDefinition = {
          |  AttributeName: List Text,
          |  AttributeType:  Types.AttributeType
          |}
          |{- Multi-line comment
          |   continued
          |-}
          |let exampleFunction : Natural -> List Natural =
          |        \(n : Natural) -> [ n, n + 1 ]
          |in { exampleFunction = exampleFunction }
          |```
        """.stripMargin

      val nl = other("\n")
      val let = keyword("let")
      val colon = other(" : ")

      parse(input) shouldBe result("dhall",
        comment("-- Single-line comment\n"),
        let, space, declName("txt"), equals, string("\"some "), escape("\\t"), string(" text\""), nl,
        let, space, declName("multilineText"), other(" =\n  "),
        string("''\n  Multi-line\n  "), subst("${txt}"), string("\n  text\n  ''"), nl,
        let, space, declName("bool"), equals, id("True"), nl,
        let, space, declName("renderedBool"), colon, typeName("Text"), equals, keyword("if"), space, id("bool"), space, keyword("then"), space, string("\"True\""), space, keyword("else"), space, string("\"False\""), nl,
        let, space, declName("naturalNumber"), colon, typeName("Natural"), equals, number("42"), nl,
        let, space, declName("positiveInteger"), colon, typeName("Integer"), equals, number("+55"), nl,
        let, space, declName("negativeInteger"), colon, typeName("Integer"), equals, number("-12"), nl,
        let, space, declName("pi"), colon, typeName("Double"), equals, number("3.14159265359"), nl,
        let, space, declName("untyped"), equals, number("123"), nl,
        let, space, declName("AttributeDefinition"), other(" = {\n  "),
        attrName("AttributeName"), other(": "), typeName("List"), space, typeName("Text"), other(",\n  "),
        attrName("AttributeType"), other(":  "), id("Types"), other("."), typeName("AttributeType"), other("\n}\n"),
        comment("{- Multi-line comment\n   continued\n-}"), nl,
        let, space, declName("exampleFunction"), colon, typeName("Natural"), other(" -> "), typeName("List"), space, typeName("Natural"), other(" =\n        \\("),
        attrName("n"), colon, typeName("Natural"), other(") -> [ "), id("n"), comma, id("n"), other(" + "), number("1"), other(" ]\n"),
        keyword("in"), other(" { "), attrName("exampleFunction"), equals, id("exampleFunction"), other(" }")
      )
    }
    
    "parse an SQL document" in {
      val input =
        """# Doc
          |
          |```sql
          |SELECT DISTINCT f.bar, f.baz
          |  FROM Foo f
          |  JOIN Bar b ON f.foo_id = b.bar_id
          |  WHERE f.up = 'lol' AND f.down = 27;
          |
          |CREATE TABLE `baz`(
          |  `BAZ_ID`         int(11),
          |  `BAZ_NAME`       varchar(120),
          |  `BAZ_VALID`      BOOLEAN,
          |  PRIMARY KEY (`BAZ_ID`)
          |);
          |``` 
        """.stripMargin

      val dot = other(".")
      val nlIndent = other("\n  ")
      parse(input) shouldBe result("sql",
        keyword("SELECT"), space, keyword("DISTINCT"), space, id("f"), dot, id("bar"), other(", "), id("f"), dot, id("baz"), nlIndent,
        keyword("FROM"), space, id("Foo"), space, id("f"), nlIndent,
        keyword("JOIN"), space, id("Bar"), space, id("b"), space, keyword("ON"), space,
        id("f"), dot, id("foo_id"), equals, id("b"), dot, id("bar_id"), nlIndent,
        keyword("WHERE"), space, id("f"), dot, id("up"), equals, string("'lol'"), space, 
        keyword("AND"), space, id("f"), dot, id("down"), equals, number("27"), other(";\n\n"),
        keyword("CREATE"), space, keyword("TABLE"), space, id("`baz`"), other("(\n  "),
        id("`BAZ_ID`"), other("         "), typeName("int"), other("("), number("11"), other("),\n  "),
        id("`BAZ_NAME`"), other("       "), typeName("varchar"), other("("), number("120"), other("),\n  "),
        id("`BAZ_VALID`"), other("      "), typeName("BOOLEAN"), other(",\n  "),
        keyword("PRIMARY"), space, keyword("KEY"), other(" ("), id("`BAZ_ID`"), other(")\n);")
      )
    }

    "parse a YAML document" in {
      val input =
        """# Doc
          |
          |```yaml
          |# Source: mychart/templates/configmaps.yaml
          |apiVersion: v1
          |kind: 'ConfigMap'
          |metadata:
          |  name: port_overriden # Some comment
          |  tag: abc#def
          |data:
          |  host: "localhost"
          |  port: 9876 # That's fine
          |  timeout: 23.56
          |  null_value: null
          |  boolean:    true
          |  octal  :    0o14
          |  hex: 0x0281Cc
          |a_sequence : 
          |  - Item 1
          |  - 123
          |  - key : value
          |```
        """.stripMargin

      val nl = other("\n")
      val nl2 = other("\n  ")
      val colon = other(": ")
      parse(input) shouldBe result("yaml",
        comment("# Source: mychart/templates/configmaps.yaml\n"),
        attrName("apiVersion"), colon, string("v1"), nl,
        attrName("kind"), colon, string("'ConfigMap'"), nl,
        attrName("metadata"), other(":\n  "),
        attrName("name"), colon, string("port_overriden"), other(" "), comment("# Some comment\n"), other("  "),
        attrName("tag"), colon, string("abc#def"), nl,
        attrName("data"), other(":\n  "),
        attrName("host"), colon, string("\"localhost\""), nl2,
        attrName("port"), colon, number("9876"), other(" "), comment("# That's fine\n"), other("  "),
        attrName("timeout"), colon, number("23.56"), nl2,
        attrName("null_value"), colon, literal("null"), nl2,
        attrName("boolean"), other(":    "), boolean("true"), nl2,
        attrName("octal"), other("  :    "), number("0o14"), nl2,
        attrName("hex"), colon, number("0x0281Cc"), nl,
        attrName("a_sequence"), other(" : \n  - "),
        string("Item 1"), other("\n  - "),
        number("123"), other("\n  - "),
        attrName("key"), other(" : "), string("value")
      )
    }
    
    "parse an EBNF document" in {
      val input =
        """# Doc
          |
          |```ebnf
          |FunType      ::=  FunArgTypes (‘=>’ | ‘?=>’) Type
          |               |  HKTypeParamClause ‘=>’ Type
          |FunArgTypes  ::=  InfixType
          |               |  ‘(’ [ FunArgType {‘,’ FunArgType } ] ‘)’
          |``` 
        """.stripMargin

      def lit(value: String): CodeSpan = string(s"‘$value’")
      def punct(content: String): CodeSpan = CodeSpan(content, CodeCategory.Tag.Punctuation)
      parse(input) shouldBe result("ebnf",
        declName("FunType"), other("      "), punct("::="), other("  "), id("FunArgTypes"), other(" ("),
        lit("=>"), other(" | "), lit("?=>"), other(") "), id("Type"), other("\n               |  "),
        id("HKTypeParamClause"), space, lit("=>"), space, id("Type"), other("\n"),
        declName("FunArgTypes"), other("  "), punct("::="), other("  "), id("InfixType"), other("\n               |  "),
        lit("("), other(" [ "), id("FunArgType"), other(" {"), lit(","), space, id("FunArgType"), other(" } ] "), lit(")")
      )
    }

    "parse a Markdown document" in new MarkupFormats {
      val input =
        """# Doc
          |
          |```md
          |Some *em* text **and** some `literal` text, an ![image](foo.jpg).
          |A [link](http://foo/), another [link][ref] and one more [ref].
          |
          |Header
          |======
          |
          |[ref]: http://foo
          |
          |### Header
          |
          |> Quote
          |```
        """.stripMargin

      parse(input) shouldBe result("md",
        txt("Some "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), string("`literal`"),
        txt(" text, an "), linkText("![image]"),  linkTarget("(foo.jpg)"), txt(".\nA "),
        linkText("[link]"), linkTarget("(http://foo/)"), txt(", another "), linkText("[link]"), linkTarget("[ref]"), txt(" and one more "), linkTarget("[ref]"), txt(".\n\n"),
        header("Header\n======"), txt("\n\n"), id("[ref]:"), linkTarget(" http://foo"), txt("\n\n"),
        header("### Header"), txt("\n\n"), 
        CodeSpan(">", CodeCategory.Markup.Quote), txt(" Quote")
      )
    }

    "parse a Markdown document with Laika extensions" in new MarkupFormats {
      val input =
        """# Doc
          |
          |```laika-md
          |{%
          |  autonumbering {
          |    scope: sections
          |    depth: 2
          |  }
          |%}
          |Some *em* text **and** some ${subst.value}.
          |
          |Header
          |======
          |
          |@:toc(foo) { depth: 2 }
          |``` 
        """.stripMargin

      parse(input) shouldBe result("laika-md",
        keyword("{%"), txt("\n  "), 
        attrName("autonumbering"), txt(" {\n    "),
        attrName("scope"), colonSpace, string("sections"), txt("\n    "),
        attrName("depth"), colonSpace, number("2"), txt("\n  }\n"),
        keyword("%}"),
        txt("\nSome "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), subst("${subst.value}"), txt(".\n\n"),
        header("Header\n======"), txt("\n\n"), 
        keyword("@:"), id("toc"), other("("), string("foo"), other(") { "), attrName("depth"), colonSpace, number("2"), other(" }")
      )
    }

    "parse a reStructuredText document" in new MarkupFormats {
      val input =
        """# Doc
          |
          |```rst
          |Some *em* text **and** some ``literal`` text.
          |And a |subst| plus a [#note]_ and `one more ref`_ and `role text`
          |and _`internal target`.
          |
          |Header1
          |=======
          |
          |++++++++
          |Header 2
          |++++++++
          |
          |.. _ref: http://foo
          |
          |.. |subs| dir::
          |
          |.. [#label] footnote
          |
          |___
          |
          |.. dir::
          | :name: value
          |``` 
        """.stripMargin

      parse(input) shouldBe result("rst",
        txt("Some "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), string("``literal``"),
        txt(" text.\nAnd a "), subst("|subst|"), txt(" plus a "), linkTarget("[#note]_"), txt(" and "),
        linkTarget("`one more ref`_"), txt(" and "), subst("`role text`"),
        txt("\nand "), id("_`internal target`"), txt(".\n\n"),
        header("Header1\n======="), txt("\n\n"), 
        header("++++++++\nHeader 2\n++++++++"),
        txt("\n\n.. "), linkTarget("_ref:"), txt(" http://foo\n\n.. "),
        subst("|subs|"), space, id("dir::"),
        txt("\n\n.. "), linkTarget("[#label]"), txt(" footnote\n\n"),
        CodeSpan("___", CodeCategory.Markup.Fence), 
        txt("\n\n.. "), id("dir"), keyword("::"), txt("\n "),
        attrName(":name:"), txt(" value")
      )
    }

    "parse a reStructuredText document with Laika extensions" in new MarkupFormats {
      val input =
        """# Doc
          |
          |```laika-rst
          |{%
          |  autonumbering {
          |    scope: sections
          |    depth: 2
          |  }
          |%}
          |Some *em* text **and** some ${subst.value}.
          |
          |Header
          |======
          |
          |@:toc(foo) { depth: 2 }
          |``` 
        """.stripMargin

      parse(input) shouldBe result("laika-rst",
        keyword("{%"), txt("\n  "),
        attrName("autonumbering"), txt(" {\n    "),
        attrName("scope"), colonSpace, string("sections"), txt("\n    "),
        attrName("depth"), colonSpace, number("2"), txt("\n  }\n"),
        keyword("%}"),
        txt("\nSome "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), subst("${subst.value}"), txt(".\n\n"),
        header("Header\n======"), txt("\n\n"),
        keyword("@:"), id("toc"), other("("), string("foo"), other(") { "), attrName("depth"), colonSpace, number("2"), other(" }")
      )
    }

    "parse an HTML document with Laika extensions" in new TagFormats {
      val input =
        """# Doc
          |
          |```laika-html
          |{%
          |  autonumbering {
          |    scope: sections
          |    depth: 2
          |  }
          |%}
          |<html>
          |  <body>
          |    @:toc { depth: 2 }
          |    <div class="content">
          |      ${cursor.currentDocument.content}
          |    </div>
          |  </body>
          |</html>
          |``` 
        """.stripMargin

      parse(input) shouldBe result("laika-html",
        keyword("{%"), other("\n  "),
        attrName("autonumbering"), other(" {\n    "),
        attrName("scope"), colonSpace, CodeSpan("sections", CodeCategory.StringLiteral), other("\n    "),
        attrName("depth"), colonSpace, number("2"), other("\n  }\n"),
        keyword("%}"), other("\n"),
        open, tagName("html"), close, nl(2),
        open, tagName("body"), close, nl(4),
        keyword("@:"), id("toc"), other(" { "), attrName("depth"), colonSpace, number("2"), other(" }\n    "),
        open, tagName("div"), spacePunct, attrName("class"), eq, qstring("content"), close, nl(6),
        subst("${cursor.currentDocument.content}"), nl(4),
        punct("</"), tagName("div"), close, nl(2),
        punct("</"), tagName("body"), close, nl(0),
        punct("</"), tagName("html"), close
      )
    }

    "parse a Laika AST document" in new TagFormats {
      val input =
        """# Doc
          |
          |```laika-ast
          |RootElement - Blocks: 2
          |. Title(Id(title) + Styles(title)) - Spans: 1
          |. . Text - 'This is the Title'
          |. Paragraph - Spans: 1
          |. . Text - 'text|second line [...] the end'
          |```
        """.stripMargin

      override def qstring(value: String): CodeSpan = CodeSpan(value, StringLiteral)
      def header(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Headline)
      val nl = other("\n")
      parse(input) shouldBe result("laika-ast",
        typeName("RootElement"), other(" - "), keyword("Blocks"), other(": "), number("2"), nl,
        punct(". "), header("Title"), other("("), id("Id"), other("("), qstring("title"), other(") + "), 
        id("Styles"), other("("), qstring("title"), other(")) - "), keyword("Spans"), other(": "), number("1"), nl,
        punct(". . "), typeName("Text"), other(" - "), qstring("'This is the Title'"), nl,
        punct(". "), typeName("Paragraph"), other(" - "), keyword("Spans"), other(": "), number("1"), nl,
        punct(". . "), typeName("Text"), other(" - "), qstring("'text"), escape("|"), qstring("second line "),
        punct("[...]"), qstring(" the end'")
      )
    }
        
  }
  
  
}
