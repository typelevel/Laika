package laika.ast

import munit.FunSuite

class ElementContainerToStringSpec extends FunSuite {

  test("toString for ElementContainer") {
    val element  = Paragraph(Text("some"), Emphasized("em"), Text("text"))
    val expected = """Paragraph - Spans: 3
                     |. Text - 'some'
                     |. Emphasized - Spans: 1
                     |. . Text - 'em'
                     |. Text - 'text'""".stripMargin
    assertEquals(element.toString, expected)
  }

}
