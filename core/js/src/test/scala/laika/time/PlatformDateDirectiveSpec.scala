package laika.time

import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.ast.{ Replace, RewriteRules, RootElement, TemplateRoot, TemplateSpan, TemplateString }
import laika.directive.std.{ MarkupParserSetup, TemplateParserSetup }
import munit.FunSuite

import scala.scalajs.js
import scala.util.Try

class PlatformDateDirectiveSpec extends FunSuite
    with ParagraphCompanionShortcuts
    with TemplateParserSetup
    with MarkupParserSetup
    with TestSourceBuilders {

  private val testDate = "2012-01-01T12:30:00+03:00"

  private lazy val longTZ = runTZ("long")
  private lazy val fullTZ = runTZ("full")

  private def runTZ(timeStyle: String): String = {
    val dateStr = PlatformDateTime
      .formatConstant(new js.Date(testDate), timeStyle)
      .flatMap(_.toOption)
      .getOrElse("")

    normalizeWhitespace(dateStr)
      .split(":00 (AM )?")
      .last
  }

  def normalizeWhitespace(str: String): String = str.replaceAll("\\p{javaSpaceChar}+", " ")

  def normalizeWhitespace(rootElement: RootElement): RootElement =
    rootElement.rewriteChildren(RewriteRules.forTemplates { case ts: TemplateString =>
      Replace(TemplateString(normalizeWhitespace(ts.content)))
    })

  def runTemplate(input: String, config: String, expectedContent: TemplateSpan*)(implicit
      loc: munit.Location
  ): Unit = {
    assertEquals(
      parseTemplateWithConfig(input, config).map(normalizeWhitespace),
      Right(RootElement(TemplateRoot(expectedContent)))
    )
  }

  private def run(
      dateStyle: String,
      expectedFormattedDate: String,
      language: String = "en",
      extraDirectiveArg: String = ""
  )(implicit loc: munit.Location): Unit = {
    val input = s"""aa @:date(laika.metadata.datePublished, $dateStyle$extraDirectiveArg) bb"""
    runTemplate(
      input,
      s"""laika.metadata.datePublished = "$testDate"
         |laika.metadata.language = $language""".stripMargin,
      TemplateString("aa "),
      TemplateString(expectedFormattedDate),
      TemplateString(" bb")
    )
  }

  test("date directive - SHORT format") {
    run("SHORT", "1/1/12, 9:30 AM")
  }

  test("date directive - MEDIUM format") {
    run("MEDIUM", "Jan 1, 2012, 9:30:00 AM")
  }

  test("date directive - LONG format") {
    run(s"LONG", s"January 1, 2012 at 9:30:00 AM $longTZ")
  }

  test("date directive - LONG format - German - language from document metadata") {
    run(s"LONG", s"1. Januar 2012 um 09:30:00 $longTZ", "de")
  }

  test("date directive - LONG format - German - language from directive argument") {
    run(s"LONG", s"1. Januar 2012 um 09:30:00 $longTZ", extraDirectiveArg = ", de")
  }

  test("date directive - FULL format") {
    run("FULL", s"Sunday, January 1, 2012 at 9:30:00 AM $fullTZ")
  }

}
