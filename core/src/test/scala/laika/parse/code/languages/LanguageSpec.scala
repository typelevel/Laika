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
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.code.CodeCategory._
import laika.parse.code.{CodeCategory, CodeSpan}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class LanguageSpec extends WordSpec with Matchers {

  
  "The syntax highlighter for code blocks" should {
    
    def parse (input: String): RootElement = 
      MarkupParser.of(Markdown).using(GitHubFlavor).build.parse(input).toOption.get.content
    
    val space: CodeSpan = CodeSpan(" ")
    val colonSpace: CodeSpan = CodeSpan(": ")
    val comma: CodeSpan = CodeSpan(", ")
    val equals: CodeSpan = CodeSpan(" = ")
    val dot: CodeSpan = CodeSpan(".")
    
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
    def tagName(value: String): CodeSpan = CodeSpan(value, CodeCategory.XML.TagName)
    def attrName(value: String): CodeSpan = CodeSpan(value, CodeCategory.AttributeName)
    def comment(value: String): CodeSpan = CodeSpan(value, CodeCategory.Comment)
    def other(value: String): CodeSpan = CodeSpan(value)
    
    "parse Scala code" in {
      
      val input =
        """# Code
          |
          |```scala
          |case class Foo (bar: Int, baz: String) {
          |
          |  val xx = "some \t value"
          |  
          |  lazy val `y-y` = +++line 1
          |    |line 2+++.stripMargin
          |  
          |  def bag = Seq(true, null, 's', 0xff)
          |  
          |  // just a short example
          |  
          |}
          |```
        """.stripMargin.replaceAllLiterally("+++", "\"\"\"")
      
      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Code")), Styles("title") + Id("code")),
        CodeBlock("scala", Seq(
          keyword("case"), space, keyword("class"), space, typeName("Foo"), 
          other(" ("), id("bar"), colonSpace, typeName("Int"), comma, id("baz"), colonSpace, typeName("String"), other(") {\n\n  "),
          keyword("val"), space, id("xx"), equals, string("\"some "), escape("\\t"), string(" value\""), other("\n  \n  "),
          keyword("lazy"), space, keyword("val"), space, id("`y-y`"), equals,
          string("\"\"\"line 1\n    |line 2\"\"\""), dot, id("stripMargin"), other("\n  \n  "),
          keyword("def"), space, id("bag"), equals, typeName("Seq"), other("("), 
          boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), other(")\n  \n  "),
          comment("// just a short example\n"),
          other("  \n}")
        ))
      ))
      
    }

    "parse Java code" in {

      val input =
        """# Code
          |
          |```java
          |class Foo {
          |
          |  private List<Object> xx = new List<Object>(true, null, 's', 0xff, "some \t value")
          |  
          |  int calc (int bar, String baz) { return bar; }
          |  
          |  // just a short example
          |  
          |}
          |```
        """.stripMargin.replaceAllLiterally("+++", "\"\"\"")

      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Code")), Styles("title") + Id("code")),
        CodeBlock("java", Seq(
          keyword("class"), space, typeName("Foo"), other(" {\n\n  "),
          keyword("private"), space, typeName("List"), other("<"), typeName("Object"), other("> "), id("xx"), equals,
          keyword("new"), space, typeName("List"), other("<"), typeName("Object"), other(">("),
          boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), comma, 
          string("\"some "), escape("\\t"), string(" value\""), other(")\n  \n  "),
          typeName("int"), space, id("calc"), other(" ("), typeName("int"), space, id("bar"), comma, typeName("String"), space, id("baz"), other(") { "),
          keyword("return"), space, id("bar"), other("; }\n  \n  "),
          comment("// just a short example\n"),
          other("  \n}")
        ))
      ))

    }

    "parse Python code" in {

      val input =
        """# Code
          |
          |```python
          |import re
          |for test_string in ['555-1212', 'ILL-EGAL']:
          |    if re.match(r'^\d{3}-\d{4}$', test_string):
          |        print (test_string, 'is a valid US local phone number')
          |    else:
          |        print (test_string, 'rejected')
          |```
        """.stripMargin.replaceAllLiterally("+++", "\"\"\"")

      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Code")), Styles("title") + Id("code")),
        CodeBlock("python", Seq(
          keyword("import"), space, id("re"), other("\n"), 
          keyword("for"), space, id("test_string"), space, keyword("in"), other(" ["), string("'555-1212'"), comma, string("'ILL-EGAL'"), other("]:\n    "),
          keyword("if"), space, id("re"), dot, id("match"), other("("), string("r'^\\d{3}-\\d{4}$'"), comma, id("test_string"), other("):\n        "),
          keyword("print"), other(" ("), id("test_string"), comma, string("'is a valid US local phone number'"), other(")\n    "),
          keyword("else"), other(":\n        "), keyword("print"), other(" ("), id("test_string"), comma, string("'rejected'"), other(")")
        ))
      ))

    }
    
    "parse JavaScript code" in {
      val input = 
      """# Code
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
        |}""".stripMargin

      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Code")), Styles("title") + Id("code")),
        CodeBlock("js", Seq(
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
        ))
      ))
    }
    
    "parse TypeScript code" in {
      val input =
      """# Code
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
        """.stripMargin

      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Code")), Styles("title") + Id("code")),
        CodeBlock("typescript", Seq(
          keyword("import"), other(" * "), keyword("as"), space, id("React"), space, keyword("from"), space, string("\"react\""), other(";\n\n"),
          keyword("interface"), space, id("Props"), other(" {\n  "),
          id("initialUserName"), colonSpace, typeName("string"), other(";\n  "),
          id("onNameUpdated"), other(": ("), id("newName"), colonSpace, typeName("string"), other(") => "), typeName("any"), other(";\n}\n\n"),
          keyword("export"), space, keyword("const"), space, id("NameEditComponent"), other(" = ("), id("props"), colonSpace, id("Props"), other(") => {\n\n  "),
          keyword("const"), other(" ["), id("editingName"), comma, id("setEditingName"), other("] = "), 
          id("React"), dot, id("useState"), other("("), id("props"), dot, id("initialUserName"), other(");\n\n  "),
          keyword("const"), space, id("onChange"), other(" = ("), id("e"), colonSpace, id("React"), dot, id("ChangeEvent"), other("<"), id("HTMLInputElement"), other(">) => {\n    "),
          id("setEditingName"), other("("), id("e"), dot, id("target"), dot, id("value"), other(");\n  };\n\n}")
        ))
      ))
    }
    
    "parse an XML document" in {
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
        """.stripMargin

      def string(value: String): CodeSpan = CodeSpan("\"" + value + "\"", StringLiteral)
      def dtdTag(value: String): CodeSpan = CodeSpan(value, CodeCategory.XML.DTDTagName)
      def nl(indent: Int): CodeSpan = CodeSpan("\n" + (" " * indent))
      def punct(content: String): CodeSpan = CodeSpan(content, CodeCategory.XML.Punctuation)
      val open: CodeSpan = CodeSpan("<", CodeCategory.XML.Punctuation)
      val close: CodeSpan = CodeSpan(">", CodeCategory.XML.Punctuation)
      val space: CodeSpan = CodeSpan(" ", CodeCategory.XML.Punctuation)
      val eq: CodeSpan = CodeSpan("=", CodeCategory.XML.Punctuation)

      parse(input) shouldBe RootElement(Seq(
        Title(Seq(Text("Doc")), Styles("title") + Id("doc")),
        CodeBlock("xml", Seq(
          punct("<?"), tagName("xml"), space, attrName("version"), eq, string("1.0"), punct("?>"), nl(0),
          punct("<!"), dtdTag("DOCTYPE"), space, id("foo"), punct(" [\n  <!"),
          dtdTag("ELEMENT"), space, id("bar"), punct(" ("), id("baz"), punct(")>\n  <!"),
          dtdTag("ELEMENT"), space, id("baz"), punct(" ("), keyword("#PCDATA"), punct(")>\n  <!"),
          dtdTag("ATTLIST"), space, id("bar"), space, id("bar_no"), space, keyword("CDATA"), space, keyword("#REQUIRED"), punct(">\n  <!"),
          dtdTag("ENTITY"), space, id("logo"), space, keyword("PUBLIC"), punct("  "), string("-//W3C//GIF logo//EN"), space, 
          string("http://www.w3.org/logo.gif"), space, keyword("NDATA"), space, id("gif"), punct(">\n]>"), nl(0),
          open, tagName("foo"), close, nl(2),
          open, tagName("bar"), space, attrName("bar_no"), eq, string("xyz-123"), close, nl(4),
          open, tagName("baz"), close, other("Some text with "), subst("&lt;"), other(" entities "), escape("&#x20;"), punct("</"), tagName("baz"), close, nl(2),
          punct("</"), tagName("bar"), close, nl(2),
          comment("<!-- some comment -->"), nl(2),
          CodeSpan("<? some pi ?>", CodeCategory.XML.ProcessingInstruction), nl(2),
          CodeSpan("<![CDATA[some cdata content]]>", CodeCategory.XML.CData), nl(0),
          punct("</"), tagName("foo"), close
        ))
      ))
    }
    
  }
  
  
}
