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
import munit.FunSuite

/**
  * @author Jens Halm
  */
class LanguageSpec extends FunSuite {

  
  private val parser = MarkupParser
    .of(Markdown)
    .using(GitHubFlavor, SyntaxHighlighting)
    .withConfigValue(LaikaKeys.firstHeaderAsTitle, true)
    .build
  
  def parse (input: String): Either[ParserError, RootElement] = parser
    .parse(input)
    .map(_.content)
  
  val space: CodeSpan = CodeSpan(" ")
  val colonSpace: CodeSpan = CodeSpan(": ")
  val comma: CodeSpan = CodeSpan(", ")
  val eql: CodeSpan = CodeSpan(" = ")
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
  
  object TagFormats {
    def qstring(value: String): CodeSpan = CodeSpan("\"" + value + "\"", StringLiteral)
    def dtdTag(value: String): CodeSpan = CodeSpan(value, CodeCategory.XML.DTDTagName)
    def nl(indent: Int): CodeSpan = CodeSpan("\n" + (" " * indent))
    def punct(content: String): CodeSpan = CodeSpan(content, CodeCategory.Tag.Punctuation)
    val open: CodeSpan = CodeSpan("<", CodeCategory.Tag.Punctuation)
    val close: CodeSpan = CodeSpan(">", CodeCategory.Tag.Punctuation)
    val spacePunct: CodeSpan = CodeSpan(" ", CodeCategory.Tag.Punctuation)
    val punctEq: CodeSpan = CodeSpan("=", CodeCategory.Tag.Punctuation)
  }
  
  object MarkupFormats {
    def txt(content: String): CodeSpan = CodeSpan(content)
    def header(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Headline)
    def em(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Emphasized)
    def linkText(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.LinkText)
    def linkTarget(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.LinkTarget)
  }
  
  test("Scala") {
    
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
    
    val expected = result("scala",
      annotation("Thing"), space, keyword("case"), space, keyword("class"), space, typeName("Foo"),
      other(" ("), id("bar"), colonSpace, typeName("Int"), comma, id("baz"), colonSpace, typeName("String"), other(") {\n\n  "),
      keyword("val"), space, id("xx"), eql, string("\"some "), escape("\\t"), string(" value\""), other("\n  \n  "),
      keyword("lazy"), space, keyword("val"), space, id("`y-y`"), eql,
      string("\"\"\"line 1\n    |line 2\"\"\""), dot, id("stripMargin"), other("\n  \n  "),
      keyword("def"), space, declName("bag"), eql, typeName("Seq"), other("("),
      boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), comma,
      string("s\"one "), subst("$subst"), string(", two "), subst("${subst.get}"), string("\""), other(")\n  \n  "),
      comment("// just a short example\n"),
      other("  \n}")
    )
    
    assertEquals(parse(input), expected)
  }

  test("Dotty") {

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

    val expected = result("dotty",
      annotation("Thing"), space, keyword("case"), space, keyword("class"), space, typeName("Foo"),
      other(" ("), id("bar"), colonSpace, typeName("Int"), comma, id("baz"), colonSpace, typeName("String"), other(") {\n\n  "),
      keyword("opaque"), space, keyword("type"), space, typeName("Xyz"), eql, typeName("Int"), other("\n\n  "),
      keyword("given"), space, id("global"), space, keyword("as"), space, typeName("ExecutionContext"), eql, 
      keyword("new"), space, typeName("ForkJoinPool"), other("()\n\n  "),
      keyword("inline"), space, keyword("val"), space, id("xx"), eql, string("\"some "), escape("\\t"), string(" value\""), other("\n  \n  "),
      keyword("lazy"), space, keyword("val"), space, id("`y-y`"), eql,
      string("\"\"\"line 1\n    |line 2\"\"\""), dot, id("stripMargin"), other("\n  \n  "),
      keyword("def"), space, declName("bag"), other("("), keyword("using"), space, typeName("ExecutionContext"), 
      other(") = "), typeName("Seq"), other("("),
      boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), other(")\n  \n  "),
      comment("// just a short example\n"),
      other("  \n}")
    )

    assertEquals(parse(input), expected)
  }

  test("Java") {

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

    val expected = result("java",
      annotation("Thing"), space, keyword("class"), space, typeName("Foo"), other(" {\n\n  "),
      keyword("private"), space, typeName("List"), other("<"), typeName("Object"), other("> "), id("xx"), eql,
      keyword("new"), space, typeName("List"), other("<"), typeName("Object"), other(">("),
      boolean("true"), comma, literal("null"), comma, char("'s'"), comma, number("0xff"), comma, 
      string("\"some "), escape("\\t"), string(" value\""), other(")\n  \n  "),
      typeName("int"), space, id("calc"), other(" ("), typeName("int"), space, id("bar"), comma, typeName("String"), space, id("baz"), other(") { "),
      keyword("return"), space, id("bar"), other("; }\n  \n  "),
      comment("// just a short example\n"),
      other("  \n}")
    )

    assertEquals(parse(input), expected)
  }

  test("Python") {

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

    val expected = result("python",
      keyword("import"), space, id("re"), other("\n"), 
      keyword("for"), space, id("test_string"), space, keyword("in"), other(" ["), string("'555-1212'"), comma, string("'ILL-EGAL'"), other("]:\n    "),
      keyword("if"), space, id("re"), dot, id("match"), other("("), string("r'^\\d{3}-\\d{4}$'"), comma, id("test_string"), other("):\n        "),
      keyword("print"), other(" ("), id("test_string"), comma, string("'is a valid US local phone number'"), other(")\n    "),
      keyword("else"), other(":\n        "), keyword("print"), other(" ("), id("test_string"), comma, string("'rejected'"), other(")")
    )

    assertEquals(parse(input), expected)
  }
  
  test("JavaScript") {
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

    val expected = result("js",
      keyword("class"), space, id("App"), space, keyword("extends"), space, id("Component"), other(" {\n\n  "),
      id("state"), other(" = {\n    "),
      id("lastResult"), colonSpace, keyword("this"), dot, id("message"), other("("), string("'Apocalypse started'"), other(")\n  }\n\n  "),
      id("handleError"), eql, id("error"), other(" => {\n    "),
      id("console"), dot, id("log"), other("("), id("error"), other(");\n    "),
      keyword("const"), space, id("msg"), other(" = ("), id("error"), dot, id("response"), other(") ? "),
      string("`Status: "), subst("${error.response.status}"), string("`"), other(" : "), string("'Unknown error'"), other(";\n    "),
      keyword("this"), dot, id("setState"), other("({ "), id("lastResult"), colonSpace, 
      keyword("this"), dot, id("message"), other("("), string("`Server Error ("), subst("${msg}"), string(")`"), 
      other(") });\n  }\n}")
    )

    assertEquals(parse(input), expected)
  }

  test("JSX") {
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

    import TagFormats._
    val expected = result("jsx",
      keyword("class"), space, id("App"), space, keyword("extends"), space, id("Component"), other(" {\n\n  "),
      id("state"), other(" = {\n    "),
      id("lastResult"), colonSpace, keyword("this"), dot, id("message"), other("("), string("'Apocalypse started'"), other(")\n  }\n\n  "),
      id("tag"), eql, punct("<"), tagName("div"), spacePunct, attrName("className"), punctEq,
      string("\"big\""), punct("><"), typeName("Comp.Hello"), punct(">"), other("See "), subst("{props.foo}"),
      punct("</"), typeName("Comp.Hello"), punct("></"), tagName("div"), punct(">"), other("\n\n  "),
      id("handleError"), eql, id("error"), other(" => {\n    "),
      id("console"), dot, id("log"), other("("), id("error"), other(");\n    "),
      keyword("const"), space, id("msg"), other(" = ("), id("error"), dot, id("response"), other(") ? "),
      string("`Status: "), subst("${error.response.status}"), string("`"), other(" : "), string("'Unknown error'"), other(";\n    "),
      keyword("this"), dot, id("setState"), other("({ "), id("lastResult"), colonSpace,
      keyword("this"), dot, id("message"), other("("), string("`Server Error ("), subst("${msg}"), string(")`"),
      other(") });\n  }\n}")
    )

    assertEquals(parse(input), expected)
  }

  
  test("Dart") {
    val input = 
    """#Doc
      |
      |```dart
      |import 'package:http/http.dart' as http;
      |
      |mixin Foo on App {
      |  void g(List<int> args) => null;
      |}
      |
      |extension Bar on App {
      |
      |}
      |
      |@Thing
      |abstract class App extends Component with Foo {
      |  const App({required this.i}) : super();
      |  final int i = 1;
      |  const color = Color(0xffb74093);
      |  const multiline = '''line 0
      |line 1
      |line 2''';
      |  const rawString = r"${'substitution'}";
      |  const str = "${'substitution'}";
      |
      |  Future<void> f(List<String> args,{required dynamic arg0},[int? arg1]) async {
      |    try {
      |      await doSomething();
      |    } on Exception catch(e) {
      |      rethrow;
      |    } finally {
      |      yield null;
      |    }
      |}
      |```""".stripMargin

      val expected = result("dart",
      id("import"),space,string("'package:http/http.dart'"),space,
      id("as"),space,id("http"),other(";\n\n"),
      id("mixin"),space,typeName("Foo"),space,id("on"),space,
      typeName("App"),other(" {\n  "),
      typeName("void"),space,id("g"),other("("),typeName("List"),other("<"),typeName("int"),other("> "),
      id("args"),other(") => "),literal("null"),other(";\n}\n\n"),
      id("extension"),space,typeName("Bar"),space,
      id("on"),space,typeName("App"),other(" {\n\n}\n\n"),
      annotation("Thing"),other("\n"),
      id("abstract"),space,keyword("class"),space,typeName("App"),space,
      keyword("extends"),space,typeName("Component"),
      space,keyword("with"),space,typeName("Foo"),other(" {\n  "),
      keyword("const"),space,typeName("App"),other("({"),
      id("required"),space,keyword("this"),other("."),id("i"),other("}) : "),keyword("super"),other("();\n  "),
      keyword("final"),space,typeName("int"),space,id("i"),other(" = "),number("1"),other(";\n  "),
      keyword("const"),space,id("color"),other(" = "),typeName("Color"),other("("),number("0xffb74093"),other(");\n  "),
      keyword("const"),space,id("multiline"),other(" = "),string("'''line 0\nline 1\nline 2'''"),other(";\n  "),
      keyword("const"),space,id("rawString"),other(" = "),string("""r"${'substitution'}""""),other(";\n  "),
      keyword("const"),space,id("str"),other(" = "),string("\""),subst("${'substitution'}"),string("\""),other(";\n\n  "),
      typeName("Future"),other("<"),typeName("void"),other("> "),id("f"),other("("),
      typeName("List"),other("<"),typeName("String"),other("> "),id("args"),
      other(",{"),id("required"),space,typeName("dynamic"),space,id("arg0"),other("},["),
      typeName("int"),other("? "),id("arg1"),other("]) "),id("async"),other(" {\n    "),
      keyword("try"),other(" {\n      "),
      id("await"),space,id("doSomething"),other("();\n    } "),
      id("on"),space,typeName("Exception"),space,keyword("catch"),other("("),id("e"),other(") {\n      "),
      keyword("rethrow"),other(";\n    } "),
      keyword("finally"),other(" {\n      "),id("yield"),space,literal("null"),other(";\n    }\n}")
      )
      assertEquals(parse(input),expected)
    }

  test("Haskell") {
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
    val expected = result("hs",
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

    assertEquals(parse(input), expected)
  }
  
  test("TypeScript") {
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

    val expected = result("typescript",
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

    assertEquals(parse(input), expected)
  }

  test("TSX") {
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

    import TagFormats._
    val expected = result("tsx",
      keyword("import"), other(" * "), keyword("as"), space, id("React"), space, keyword("from"), space, string("\"react\""), other(";\n\n"),
      keyword("interface"), space, id("Props"), other(" {\n  "),
      id("initialUserName"), colonSpace, typeName("string"), other(";\n  "),
      id("onNameUpdated"), other(": ("), id("newName"), colonSpace, typeName("string"), other(") => "), typeName("any"), other(";\n}\n\n"),
      keyword("export"), space, keyword("const"), space, id("NameEditComponent"), other(" = ("), id("props"), colonSpace, id("Props"), other(") => {\n\n  "),
      keyword("const"), other(" ["), id("editingName"), comma, id("setEditingName"), other("] = "),
      id("React"), dot, id("useState"), other("("), id("props"), dot, id("initialUserName"), other(");\n\n  "),
      keyword("const"), space, id("tag"), eql, punct("<"), tagName("div"), spacePunct, attrName("className"), punctEq,
      string("\"big\""), punct("><"), typeName("Comp.Hello"), punct(">"), other("See "), subst("{props.foo}"),
      punct("</"), typeName("Comp.Hello"), punct("></"), tagName("div"), punct(">"), other(";\n\n  "),
      keyword("const"), space, id("onChange"), other(" = ("), id("e"), colonSpace, id("React"), dot, id("ChangeEvent"), other("<"), id("HTMLInputElement"), other(">) => {\n    "),
      id("setEditingName"), other("("), id("e"), dot, id("target"), dot, id("value"), other(");\n  };\n\n}")
    )

    assertEquals(parse(input), expected)
  }

  test("Alloy") {
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
    val expected = result("alloy",
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

    assertEquals(parse(input), expected)
  }
  
  test("XML") {
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

    import TagFormats._
    val expected = result("xml",
      punct("<?"), tagName("xml"), spacePunct, attrName("version"), punctEq, qstring("1.0"), punct("?>"), nl(0),
      punct("<!"), dtdTag("DOCTYPE"), spacePunct, id("foo"), punct(" [\n  <!"),
      dtdTag("ELEMENT"), spacePunct, id("bar"), punct(" ("), id("baz"), punct(")>\n  <!"),
      dtdTag("ELEMENT"), spacePunct, id("baz"), punct(" ("), keyword("#PCDATA"), punct(")>\n  <!"),
      dtdTag("ATTLIST"), spacePunct, id("bar"), spacePunct, id("bar_no"), spacePunct, keyword("CDATA"), spacePunct, keyword("#REQUIRED"), punct(">\n  <!"),
      dtdTag("ENTITY"), spacePunct, id("logo"), spacePunct, keyword("PUBLIC"), punct("  "), qstring("-//W3C//GIF logo//EN"), spacePunct,
      qstring("http://www.w3.org/logo.gif"), spacePunct, keyword("NDATA"), spacePunct, id("gif"), punct(">\n]>"), nl(0),
      open, tagName("foo"), close, nl(2),
      open, tagName("bar"), spacePunct, attrName("bar_no"), punctEq, qstring("xyz-123"), close, nl(4),
      open, tagName("baz"), close, other("Some text with "), subst("&lt;"), other(" entities "), escape("&#x20;"), punct("</"), tagName("baz"), close, nl(2),
      punct("</"), tagName("bar"), close, nl(2),
      comment("<!-- some comment -->"), nl(2),
      CodeSpan("<? some pi ?>", CodeCategory.XML.ProcessingInstruction), nl(2),
      CodeSpan("<![CDATA[some cdata content]]>", CodeCategory.XML.CData), nl(0),
      punct("</"), tagName("foo"), close
    )

    assertEquals(parse(input), expected)
  }

  test("HTML") {
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

    import TagFormats._
    val expected = result("html",
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
      open, tagName("script"), spacePunct, attrName("src"), punctEq, qstring("foo.js"), punct("/>"), nl(4),
      open, tagName("p"), spacePunct, attrName("class"), punctEq, qstring("big"), close, other("Some text with "),
      subst("&lt;"), other(" entities "), escape("&#x20;"), punct("</"), tagName("p"), close, nl(4),
      comment("<!-- some comment -->"), nl(2),
      punct("</"), tagName("body"), close, nl(0),
      punct("</"), tagName("html"), close
    )

    assertEquals(parse(input), expected)
  }

  test("CSS") {
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

    val expected = result("css",
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

    assertEquals(parse(input), expected)
  }

  test("JSON") {
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

    val expected = result("json",
      other("{\n  "),
      attrName("foo"), other(": ["), boolean("false"), comma, number("2.3e-2"), comma, literal("null"), comma, string("\"bar\""),
      other(", { "), attrName("nested"), colonSpace, boolean("true"), other(" }\n}")
    )

    assertEquals(parse(input), expected)
  }

  test("HOCON") {
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
    
    val expected = result("hocon",
      other("{\n  "),
      attrName("a"), other(" = ["), boolean("false"), comma, number("2.3e-2"), comma, literal("null"), comma, string("\"bar\""),
      other(", { "), attrName("nested"), eql, boolean("true"), nl(" }"),
      keyword("include"), space, id("required"), other("("), id("file"), other("("), string("\"xx.conf\""), nl("))"),
      attrName("b"), eql, string("unquoted string"), space, comment("# comment\n"), other("  \n  "),
      attrName("\"c\""), other(" : "), string("text"), space, subst("${subst}"), space, string("text"), nl(""),
      attrName("d e f"), eql, multiline("multiline\n          string"), other("\n}"),
    )

    assertEquals(parse(input), expected)
  }

  test("Dhall") {
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

    val expected = result("dhall",
      comment("-- Single-line comment\n"),
      let, space, declName("txt"), eql, string("\"some "), escape("\\t"), string(" text\""), nl,
      let, space, declName("multilineText"), other(" =\n  "),
      string("''\n  Multi-line\n  "), subst("${txt}"), string("\n  text\n  ''"), nl,
      let, space, declName("bool"), eql, id("True"), nl,
      let, space, declName("renderedBool"), colon, typeName("Text"), eql, keyword("if"), space, id("bool"), space, keyword("then"), space, string("\"True\""), space, keyword("else"), space, string("\"False\""), nl,
      let, space, declName("naturalNumber"), colon, typeName("Natural"), eql, number("42"), nl,
      let, space, declName("positiveInteger"), colon, typeName("Integer"), eql, number("+55"), nl,
      let, space, declName("negativeInteger"), colon, typeName("Integer"), eql, number("-12"), nl,
      let, space, declName("pi"), colon, typeName("Double"), eql, number("3.14159265359"), nl,
      let, space, declName("untyped"), eql, number("123"), nl,
      let, space, declName("AttributeDefinition"), other(" = {\n  "),
      attrName("AttributeName"), other(": "), typeName("List"), space, typeName("Text"), other(",\n  "),
      attrName("AttributeType"), other(":  "), id("Types"), other("."), typeName("AttributeType"), other("\n}\n"),
      comment("{- Multi-line comment\n   continued\n-}"), nl,
      let, space, declName("exampleFunction"), colon, typeName("Natural"), other(" -> "), typeName("List"), space, typeName("Natural"), other(" =\n        \\("),
      attrName("n"), colon, typeName("Natural"), other(") -> [ "), id("n"), comma, id("n"), other(" + "), number("1"), other(" ]\n"),
      keyword("in"), other(" { "), attrName("exampleFunction"), eql, id("exampleFunction"), other(" }")
    )

    assertEquals(parse(input), expected)
  }

  test("shell script") {
    val input = """# Doc
        |
        |```sh
        |# this is a comment
        |export KEY=value # this is a trailing comment
        |echo 'hello world'
        |
        |echo hello $(whoami)
        |
        |for i in ${array[@]}; do
        |  read -p "" input
        |  echo "\"${input}\""
        |done
        |
        |exit
        |```
        |
        |""".stripMargin
    val parsed = parse(input)
    val expected = result("sh",
      comment("# this is a comment\n"),
      keyword("export"),space,id("KEY"),other("="),id("value"),space,comment("# this is a trailing comment\n"),
      keyword("echo"),space,string("'hello world'"),
      other("\n\n"),
      keyword("echo"),space,id("hello"),space,subst("$(whoami)"),other("\n\n"),
      keyword("for"),space,id("i"),space,id("in"),space,subst("${array[@]}"),other("; "),keyword("do"),other("\n  "),
      keyword("read"),other(" -"),id("p"),space,string("\"\""),space,id("input"),other("\n  "),
      keyword("echo"),space,string("\""),escape("\\\""),subst("${input}"),escape("\\\""),string("\""),
      other("\n"),
      keyword("done"),
      other("\n\n"),
      keyword("exit")
    )
    assertEquals(parsed,expected)
  }
  
  test("SQL") {
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
    val expected = result("sql",
      keyword("SELECT"), space, keyword("DISTINCT"), space, id("f"), dot, id("bar"), other(", "), id("f"), dot, id("baz"), nlIndent,
      keyword("FROM"), space, id("Foo"), space, id("f"), nlIndent,
      keyword("JOIN"), space, id("Bar"), space, id("b"), space, keyword("ON"), space,
      id("f"), dot, id("foo_id"), eql, id("b"), dot, id("bar_id"), nlIndent,
      keyword("WHERE"), space, id("f"), dot, id("up"), eql, string("'lol'"), space, 
      keyword("AND"), space, id("f"), dot, id("down"), eql, number("27"), other(";\n\n"),
      keyword("CREATE"), space, keyword("TABLE"), space, id("`baz`"), other("(\n  "),
      id("`BAZ_ID`"), other("         "), typeName("int"), other("("), number("11"), other("),\n  "),
      id("`BAZ_NAME`"), other("       "), typeName("varchar"), other("("), number("120"), other("),\n  "),
      id("`BAZ_VALID`"), other("      "), typeName("BOOLEAN"), other(",\n  "),
      keyword("PRIMARY"), space, keyword("KEY"), other(" ("), id("`BAZ_ID`"), other(")\n);")
    )

    assertEquals(parse(input), expected)
  }

  test("YAML") {
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
    val expected = result("yaml",
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

    assertEquals(parse(input), expected)
  }
  
  test("EBNF") {
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
    val expected = result("ebnf",
      declName("FunType"), other("      "), punct("::="), other("  "), id("FunArgTypes"), other(" ("),
      lit("=>"), other(" | "), lit("?=>"), other(") "), id("Type"), other("\n               |  "),
      id("HKTypeParamClause"), space, lit("=>"), space, id("Type"), other("\n"),
      declName("FunArgTypes"), other("  "), punct("::="), other("  "), id("InfixType"), other("\n               |  "),
      lit("("), other(" [ "), id("FunArgType"), other(" {"), lit(","), space, id("FunArgType"), other(" } ] "), lit(")")
    )

    assertEquals(parse(input), expected)
  }

  test("Markdown") {
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

    import MarkupFormats._
    val expected = result("md",
      txt("Some "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), string("`literal`"),
      txt(" text, an "), linkText("![image]"),  linkTarget("(foo.jpg)"), txt(".\nA "),
      linkText("[link]"), linkTarget("(http://foo/)"), txt(", another "), linkText("[link]"), linkTarget("[ref]"), txt(" and one more "), linkTarget("[ref]"), txt(".\n\n"),
      header("Header\n======"), txt("\n\n"), id("[ref]:"), linkTarget(" http://foo"), txt("\n\n"),
      header("### Header"), txt("\n\n"), 
      CodeSpan(">", CodeCategory.Markup.Quote), txt(" Quote")
    )

    assertEquals(parse(input), expected)
  }

  test("Markdown with Laika extensions") {
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

    import MarkupFormats._
    val expected = result("laika-md",
      keyword("{%"), txt("\n  "), 
      attrName("autonumbering"), txt(" {\n    "),
      attrName("scope"), colonSpace, string("sections"), txt("\n    "),
      attrName("depth"), colonSpace, number("2"), txt("\n  }\n"),
      keyword("%}"),
      txt("\nSome "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), subst("${subst.value}"), txt(".\n\n"),
      header("Header\n======"), txt("\n\n"), 
      keyword("@:"), id("toc"), other("("), string("foo"), other(") { "), attrName("depth"), colonSpace, number("2"), other(" }")
    )

    assertEquals(parse(input), expected)
  }

  test("reStructuredText") {
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

    import MarkupFormats._
    val expected = result("rst",
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

    assertEquals(parse(input), expected)
  }

  test("reStructuredText with Laika extensions") {
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

    import MarkupFormats._
    val expected = result("laika-rst",
      keyword("{%"), txt("\n  "),
      attrName("autonumbering"), txt(" {\n    "),
      attrName("scope"), colonSpace, string("sections"), txt("\n    "),
      attrName("depth"), colonSpace, number("2"), txt("\n  }\n"),
      keyword("%}"),
      txt("\nSome "), em("*em*"), txt(" text "), em("**and**"), txt(" some "), subst("${subst.value}"), txt(".\n\n"),
      header("Header\n======"), txt("\n\n"),
      keyword("@:"), id("toc"), other("("), string("foo"), other(") { "), attrName("depth"), colonSpace, number("2"), other(" }")
    )

    assertEquals(parse(input), expected)
  }

  test("HTML with Laika extensions") {
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

    import TagFormats._
    val expected = result("laika-html",
      keyword("{%"), other("\n  "),
      attrName("autonumbering"), other(" {\n    "),
      attrName("scope"), colonSpace, CodeSpan("sections", CodeCategory.StringLiteral), other("\n    "),
      attrName("depth"), colonSpace, number("2"), other("\n  }\n"),
      keyword("%}"), other("\n"),
      open, tagName("html"), close, nl(2),
      open, tagName("body"), close, nl(4),
      keyword("@:"), id("toc"), other(" { "), attrName("depth"), colonSpace, number("2"), other(" }\n    "),
      open, tagName("div"), spacePunct, attrName("class"), punctEq, qstring("content"), close, nl(6),
      subst("${cursor.currentDocument.content}"), nl(4),
      punct("</"), tagName("div"), close, nl(2),
      punct("</"), tagName("body"), close, nl(0),
      punct("</"), tagName("html"), close
    )

    assertEquals(parse(input), expected)
  }

  test("Laika AST") {
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

    import TagFormats._
    def header(content: String): CodeSpan = CodeSpan(content, CodeCategory.Markup.Headline)
    val nl = other("\n")
    val expected = result("laika-ast",
      typeName("RootElement"), other(" - "), keyword("Blocks"), other(": "), number("2"), nl,
      punct(". "), header("Title"), other("("), id("Id"), other("("), string("title"), other(") + "), 
      id("Styles"), other("("), string("title"), other(")) - "), keyword("Spans"), other(": "), number("1"), nl,
      punct(". . "), typeName("Text"), other(" - "), string("'This is the Title'"), nl,
      punct(". "), typeName("Paragraph"), other(" - "), keyword("Spans"), other(": "), number("1"), nl,
      punct(". . "), typeName("Text"), other(" - "), string("'text"), escape("|"), string("second line "),
      punct("[...]"), string(" the end'")
    )

    assertEquals(parse(input), expected)
  }
  
  
}
