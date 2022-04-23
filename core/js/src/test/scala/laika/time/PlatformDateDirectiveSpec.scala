package laika.time

import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.ast.{RootElement, TemplateRoot, TemplateSpan, TemplateString}
import laika.directive.std.{MarkupParserSetup, TemplateParserSetup}
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

  private def runTZ(timeStyle: String): String =
    PlatformDateTime
      .formatConstant(new js.Date(testDate), timeStyle)
      .flatMap(_.toOption)
      .getOrElse("")
      .split("00 AM ")
      .last
    
  def runTemplate (input: String, config: String, expectedContent: TemplateSpan*)(implicit loc: munit.Location): Unit = {
    assertEquals(parseTemplateWithConfig(input, config), Right(RootElement(TemplateRoot(expectedContent))))
  }

  private def run(dateStyle: String, expectedFormattedDate: String)(implicit loc: munit.Location): Unit = {
    val input = s"""aa @:date(laika.metadata.datePublished, $dateStyle) bb"""
    runTemplate(input,
      s"""laika.metadata.datePublished = "$testDate"""",
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

  test("date directive - FULL format") {
    run("FULL", s"Sunday, January 1, 2012 at 9:30:00 AM $fullTZ")
  }

}
