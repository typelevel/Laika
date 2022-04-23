package laika.time

import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.ast.{RootElement, TemplateRoot, TemplateSpan, TemplateString}
import laika.directive.std.{MarkupParserSetup, TemplateParserSetup}
import munit.FunSuite

import java.time.OffsetDateTime
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.util.Locale

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

  test("date directive - ISO offset date time") {
    val input = """aa @:date(laika.metadata.datePublished, ISO_OFFSET_DATE_TIME) bb"""
    runTemplate(input,
      """laika.metadata.datePublished = "2012-01-01T12:30:00+03:00"""",
      TemplateString("aa "),
      TemplateString("2012-01-01T12:30:00+03:00"),
      TemplateString(" bb")
    )
  }

  test("date directive - MEDIUM format") {
    val dateString = "2012-01-01T12:30:00+03:00"
    val input = """aa @:date(laika.metadata.datePublished, MEDIUM) bb"""
    val fmt = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withLocale(Locale.getDefault)
    val expected = fmt.format(OffsetDateTime.parse(dateString))
    runTemplate(input,
      s"""laika.metadata.datePublished = "$dateString"""",
      TemplateString("aa "),
      TemplateString(expected),
      TemplateString(" bb")
    )
  }

  test("date directive - MEDIUM format - German - language from document metadata") {
    val dateString = "2012-01-01T12:30:00+03:00"
    val input = """aa @:date(laika.metadata.datePublished, MEDIUM) bb"""
    val fmt = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withLocale(Locale.GERMAN)
    val expected = fmt.format(OffsetDateTime.parse(dateString))
    runTemplate(input,
      s"""laika.metadata.datePublished = "$dateString"
         |laika.metadata.language = de""".stripMargin,
      TemplateString("aa "),
      TemplateString(expected),
      TemplateString(" bb")
    )
  }

  test("date directive - MEDIUM format - German - language from directive argument") {
    val dateString = "2012-01-01T12:30:00+03:00"
    val input = """aa @:date(laika.metadata.datePublished, MEDIUM, de) bb"""
    val fmt = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withLocale(Locale.GERMAN)
    val expected = fmt.format(OffsetDateTime.parse(dateString))
    runTemplate(input,
      s"""laika.metadata.datePublished = "$dateString"""",
      TemplateString("aa "),
      TemplateString(expected),
      TemplateString(" bb")
    )
  }

}
