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
    
    def s (category: CodeCategory, text: String): CodeSpan = CodeSpan(text, category)
    def t (text: String): CodeSpan = CodeSpan(text)
    
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
          CodeSpan("case", Keyword),
          space,
          CodeSpan("class", Keyword),
          space,
          CodeSpan("Foo", TypeName),
          CodeSpan(" ("),
          CodeSpan("bar", Identifier),
          CodeSpan(": "),
          CodeSpan("Int", TypeName),
          comma,
          CodeSpan("baz", Identifier),
          CodeSpan(": "),
          CodeSpan("String", TypeName),
          CodeSpan(") {\n\n  "),
          CodeSpan("val", Keyword),
          space,
          CodeSpan("xx", Identifier),
          equals,
          CodeSpan("\"some ", StringLiteral),
          CodeSpan("\\t", EscapeSequence),
          CodeSpan(" value\"", StringLiteral),
          CodeSpan("\n  \n  "),
          CodeSpan("lazy", Keyword),
          space,
          CodeSpan("val", Keyword),
          space,
          CodeSpan("`y-y`", Identifier),
          equals,
          CodeSpan("\"\"\"line 1\n    |line 2\"\"\"", StringLiteral),
          CodeSpan("."),
          CodeSpan("stripMargin", Identifier),
          CodeSpan("\n  \n  "),
          CodeSpan("def", Keyword),
          space,
          CodeSpan("bag", Identifier),
          equals,
          CodeSpan("Seq", TypeName),
          CodeSpan("("),
          CodeSpan("true", BooleanLiteral),
          comma,
          CodeSpan("null", LiteralValue),
          comma,
          CodeSpan("'s'", CharLiteral),
          comma,
          CodeSpan("0xff", NumberLiteral),
          CodeSpan(")\n  \n  "),
          CodeSpan("// just a short example\n", CodeCategory.Comment),
          CodeSpan("  \n}")
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
          CodeSpan("class", Keyword),
          space,
          CodeSpan("Foo", TypeName),
          CodeSpan(" {\n\n  "),
          CodeSpan("private", Keyword),
          space,
          CodeSpan("List", TypeName),
          CodeSpan("<"),
          CodeSpan("Object", TypeName),
          CodeSpan("> "),
          CodeSpan("xx", Identifier),
          equals,
          CodeSpan("new", Keyword),
          space,
          CodeSpan("List", TypeName),
          CodeSpan("<"),
          CodeSpan("Object", TypeName),
          CodeSpan(">("),
          CodeSpan("true", BooleanLiteral),
          comma,
          CodeSpan("null", LiteralValue),
          comma,
          CodeSpan("'s'", CharLiteral),
          comma,
          CodeSpan("0xff", NumberLiteral),
          comma,
          CodeSpan("\"some ", StringLiteral),
          CodeSpan("\\t", EscapeSequence),
          CodeSpan(" value\"", StringLiteral),
          CodeSpan(")\n  \n  "),
          CodeSpan("int", TypeName),
          space,
          CodeSpan("calc", Identifier),
          CodeSpan(" ("),
          CodeSpan("int", TypeName),
          space,
          CodeSpan("bar", Identifier),
          comma,
          CodeSpan("String", TypeName),
          space,
          CodeSpan("baz", Identifier),
          CodeSpan(") { "),
          CodeSpan("return", Keyword),
          space,
          CodeSpan("bar", Identifier),
          CodeSpan("; }\n  \n  "),
          CodeSpan("// just a short example\n", CodeCategory.Comment),
          CodeSpan("  \n}")
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
          keyword("import"),
          space,
          id("re"),
          other("\n"),
          keyword("for"),
          space,
          id("test_string"),
          space,
          keyword("in"),
          other(" ["),
          string("'555-1212'"),
          comma,
          string("'ILL-EGAL'"),
          other("]:\n    "),
          keyword("if"),
          space,
          id("re"),
          dot,
          id("match"),
          other("("),
          string("r'^\\d{3}-\\d{4}$'"),
          comma,
          id("test_string"),
          other("):\n        "),
          keyword("print"),
          other(" ("),
          id("test_string"),
          comma,
          string("'is a valid US local phone number'"),
          other(")\n    "),
          keyword("else"),
          other(":\n        "),
          keyword("print"),
          other(" ("),
          id("test_string"),
          comma,
          string("'rejected'"),
          other(")")
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
    
  }
  
  
}
