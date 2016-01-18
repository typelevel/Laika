package laika.directive

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import laika.parse.helper.ParseResultHelpers
import laika.parse.helper.DefaultParserHelpers
import laika.tree.helper.ModelBuilder
import laika.template.TemplateParsers
import laika.directive.Directives.Blocks
import laika.directive.Directives.Blocks.Directive
import laika.directive.Directives.Default
import laika.directive.Directives.Spans
import laika.tree.Templates._
import laika.tree.Elements._
import laika.tree.Templates.MarkupContextReference
import laika.util.Builders._
import laika.parse.InlineParsers
import laika.parse.BlockParsers

class BlockDirectiveAPISpec extends FlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Blocks.Combinators._
    import Blocks.Converters._
    import laika.util.Builders._
    
    trait RequiredDefaultAttribute {
      val directive = Blocks.create("dir") { attribute(Default) map (p(_)) }
    }
    
    trait OptionalDefaultAttribute {
      val directive = Blocks.create("dir") { 
        attribute(Default, positiveInt).optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Blocks.create("dir") { attribute("name") map (p(_)) }
    }
    
    trait OptionalNamedAttribute {
      val directive = Blocks.create("dir") { 
        attribute("name", positiveInt).optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredDefaultBody {
      val directive = Blocks.create("dir") { body(Default) map (BlockSequence(_)) }
    }
    
    trait OptionalDefaultBody {
      val directive = Blocks.create("dir") { 
        body(Default).optional map (blocks => BlockSequence(blocks.getOrElse(Nil))) 
      }
    }
    
    trait RequiredNamedBody {
      val directive = Blocks.create("dir") { body("name") map (BlockSequence(_)) }
    }
    
    trait OptionalNamedBody {
      val directive = Blocks.create("dir") { 
        body("name").optional map (blocks => BlockSequence(blocks.getOrElse(Nil))) 
      }
    }
    
    trait FullDirectiveSpec {
      val directive = Blocks.create("dir") {
        (attribute(Default) ~ attribute("strAttr").optional ~ attribute("intAttr", positiveInt).optional ~
        body(Default) ~ body("blockBody").optional ~ body("intBody", positiveInt).optional) {
          (defAttr, strAttr, intAttr, defBody, blockBody, intBody) => 
            val sum = intAttr.getOrElse(0) + intBody.getOrElse(0)
            val str = defAttr + ":" + strAttr.getOrElse("..") + ":" + sum
            BlockSequence(p(str) +: (defBody ++ blockBody.getOrElse(Nil)))
        }
      }
    }
    
    trait DirectiveWithParserAccess {
      val directive = Blocks.create("dir") { 
        (body(Default, string) ~ parser) {
          (body, parser) => BlockSequence(parser(body.drop(3)))
        }
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Blocks.create("dir") { 
        (body(Default, string) ~ context) {
          (body, context) => p(body + context.document.path)
        }
      }
    }
    
  }
  
  trait EmptyBlockParsers extends BlockParsers with InlineParsers {
    override protected def prepareSpanParsers = Map[Char,Parser[Span]]()
    override protected def prepareBlockParsers (nested: Boolean): List[Parser[Block]] = List(
      ((not(blankLine) ~> restOfLine) +) ^^ { lines => Paragraph(parseInline(lines mkString "\n")) }
    )
  }
  trait TemplateParser extends EmptyBlockParsers 
                          with TemplateParsers.MarkupBlocks
                          with TemplateParsers.MarkupSpans
                          with ParseResultHelpers 
                          with DefaultParserHelpers[RootElement] {
    
    val directive: Directive
    
    def getBlockDirective (name: String): Option[Blocks.Directive] =
      if (directive.name == name) Some(directive) else None
    
    def getSpanDirective (name: String): Option[Spans.Directive] = None
    
    val defaultParser: Parser[RootElement] = rootElement
    
    def invalid (input: String, error: String): InvalidBlock = 
        InvalidBlock(SystemMessage(laika.tree.Elements.Error, error), LiteralBlock(input))
        
    def nonRecursiveBlock: Parser[Block] = success(Paragraph(Nil)) // not used in these tests
  }
  

  import DirectiveSetup._
  
  "The directive parser" should "parse a directive with one required default string attribute" in {
    new RequiredDefaultAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir foo.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }
  
  it should "detect a directive with a missing required default attribute" in {
    new RequiredDefaultAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with an optional default int attribute" in {
    new OptionalDefaultAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir 5.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }
  
  it should "detect a directive with an optional invalid default int attribute" in {
    new OptionalDefaultAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting default attribute: not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir foo.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with a missing optional default int attribute" in {
    new OptionalDefaultAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }
  
  it should "parse a directive with one required named string attribute" in {
    val input = """aa
        |
        |@:dir name=foo.
        |
        |bb""".stripMargin
    new RequiredNamedAttribute with TemplateParser {
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }
  
  it should "parse a directive with a named string attribute value in quotes" in {
    val input = """aa
        |
        |@:dir name="foo bar".
        |
        |bb""".stripMargin
    new RequiredNamedAttribute with TemplateParser {
      Parsing (input) should produce (root(p("aa"), p("foo bar"), p("bb")))
    }
  }
  
  it should "detect a directive with a missing required named attribute" in {
    new RequiredNamedAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required attribute with name 'name' is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with an optional named int attribute" in {
    new OptionalNamedAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir name=5.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }
  
  it should "detect a directive with an optional invalid named int attribute" in {
    new OptionalNamedAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir name=foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting attribute with name 'name': not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir name=foo.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with a missing optional named int attribute" in {
    new OptionalNamedAttribute with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }
  
  it should "parse a directive with a required default body on the same line" in {
    new RequiredDefaultBody with TemplateParser {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a directive with a required default body on the following lines" in {
    new RequiredDefaultBody with TemplateParser {
      val input = """aa
        |
        |@:dir: 
        |  some 
        |  {{ref}} 
        |  text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some\n"), MarkupContextReference("ref"), txt("\ntext"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "detect a directive with a missing required default body" in {
    new RequiredDefaultBody with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with an optional default body" in {
    new OptionalDefaultBody with TemplateParser {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a directive with a missing optional default body" in {
    new OptionalDefaultBody with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), BlockSequence(Nil), p("bb")))
    }
  }
  
  it should "parse a directive with a required named body" in {
    new RequiredNamedBody with TemplateParser {
      val input = """aa
        |
        |@:dir: ~name: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "detect a directive with a missing required named body" in {
    new RequiredNamedBody with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required body with name 'name' is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with an optional named body" in {
    new OptionalNamedBody with TemplateParser {
      val input = """aa
        |
        |@:dir: ~name: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a directive with a missing optional named body" in {
    new OptionalNamedBody with TemplateParser {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), BlockSequence(Nil), p("bb")))
    }
  }
  
  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo strAttr=str intAttr=7: 
        |  1 {{ref1}} 2
        |~blockBody:
        |  3 {{ref3}} 4
        |~intBody: 9
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:str:16"), 
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2")), 
        p(txt("3 "), MarkupContextReference("ref3"), txt(" 4"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a full directive spec with all elements present and blank lines between blocks" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo strAttr=str intAttr=7: 
        |  1 {{ref1}} 2
        |
        |~blockBody:
        |  3 {{ref3}} 4
        |
        |~intBody: 9
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:str:16"), 
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2")), 
        p(txt("3 "), MarkupContextReference("ref3"), txt(" 4"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo: 
        |  1 {{ref1}} 2
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:..:0"), 
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "detect a full directive spec with one required attribute and one required body missing" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir strAttr=str.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing, required default body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir strAttr=str.",msg), p("bb")))
    }
  }
  
  it should "parse a directive with a required default body and parser access" in {
    new DirectiveWithParserAccess with TemplateParser {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("e "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }
  
  it should "parse a directive with a required default body and context access" in {
    new DirectiveWithContextAccess with TemplateParser {
      val input = """aa
        |
        |@:dir: text
        |
        |bb""".stripMargin
      def translate (result: RootElement) = result rewrite {
        case d: DirectiveBlock => Some(p("ok")) // cannot compare DirectiveSpans
      }
      Parsing (input) map translate should produce (root(p("aa"), p("ok"), p("bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new OptionalNamedAttribute with TemplateParser {
      val input = """aa
        |
        |@:foo name=foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'foo': No block directive registered with name: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:foo name=foo.",msg), p("bb")))
    }
  }
  
  
}
