package laika.time

import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.ast.{RootElement, TemplateRoot, TemplateSpan, TemplateString}
import laika.directive.std.{MarkupParserSetup, TemplateParserSetup}
import munit.FunSuite

class PlatformDateDirectiveSpec extends FunSuite
  with ParagraphCompanionShortcuts
  with TemplateParserSetup
  with MarkupParserSetup
  with TestSourceBuilders {

  def runTemplate (input: String, config: String, expectedContent: TemplateSpan*)(implicit loc: munit.Location): Unit = {
    assertEquals(parseTemplateWithConfig(input, config), Right(RootElement(TemplateRoot(expectedContent))))
  }

  test("date directive - full offset date time") {
    val input = """aa @:date(laika.metadata.datePublished, yyyy-MM-dd'T'HH:mm:ssX) bb"""
    runTemplate(input,
      """laika.metadata.datePublished = "2012-01-01T12:30:00+03:00"""",
      TemplateString("aa "),
      TemplateString("2012-01-01T12:30:00+03"),
      TemplateString(" bb")
    )
  }

  test("date directive - input with time component - format without") {
    val input = """aa @:date(laika.metadata.datePublished, yyyy-MM-dd) bb"""
    runTemplate(input,
      """laika.metadata.datePublished = "2012-01-01T12:30:00"""",
      TemplateString("aa "),
      TemplateString("2012-01-01"),
      TemplateString(" bb")
    )
  }

  test("date directive - input without time component - format with") {
    val input = """aa @:date(laika.metadata.datePublished, yyyy-MM-dd'T'HH:mm:ss) bb"""
    runTemplate(input,
      """laika.metadata.datePublished = "2012-01-01"""",
      TemplateString("aa "),
      TemplateString("2012-01-01T00:00:00"),
      TemplateString(" bb")
    )
  }
  
}
