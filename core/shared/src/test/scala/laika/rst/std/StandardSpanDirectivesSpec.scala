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

package laika.rst.std

import java.util.Date

import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast.RelativePath.Current
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.ReStructuredText
import laika.time.PlatformDateFormat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author Jens Halm
 */
class StandardSpanDirectivesSpec extends AnyFlatSpec 
                                  with Matchers 
                                  with ModelBuilder {


  def parse (input: String): RootElement = MarkupParser.of(ReStructuredText).build.parse(input).toOption.get.content
  
  val imgPath = Some(LinkPath(Root / "picture.jpg", Current / "picture.jpg"))
  
  "The image directive" should "parse the URI argument" in {
    val input = """.. |subst| image:: picture.jpg
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),img("", "picture.jpg", imgPath)))
    parse(input) should be (result)
  }
  
  it should "support the alt option" in {
    val input = """.. |subst| image:: picture.jpg
      | :alt: alt
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),img("alt", "picture.jpg", imgPath)))
    parse(input) should be (result)
  }
  
  it should "support the target option with a simple reference" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: ref_
      |
      |.. _ref: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a phrase reference" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: `some ref`_
      |
      |.. _`some ref`: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the target option with a uri" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    parse(input) should be (result)
  }
  
  it should "support the class option" in {
    val input = """.. |subst| image:: picture.jpg
      | :class: foo
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),Image("",URI("picture.jpg", imgPath),options=Styles("foo"))))
    parse(input) should be (result)
  }

  it should "support the align option" in {
    val input = """.. |subst| image:: picture.jpg
                  | :align: top
                  |
                  |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),Image("",URI("picture.jpg", imgPath),options=Styles("align-top"))))
    parse(input) should be (result)
  }

  it should "support the width and height option" in {
    val input = """.. |subst| image:: picture.jpg
                  | :width: 200px
                  | :height: 120px
                  |
                  |Some |subst|""".stripMargin
    val expectedWidth = Some(Size(200, "px"))
    val expectedHeight = Some(Size(120, "px"))
    val result = root (p(Text("Some "),Image("",URI("picture.jpg", imgPath),width = expectedWidth,height = expectedHeight)))
    parse(input) should be (result)
  }

  it should "support the scale option" in {
    val input = """.. |subst| image:: picture.jpg
                  | :width: 200 px
                  | :height: 120 px
                  | :scale: 50%
                  |
                  |Some |subst|""".stripMargin
    val expectedWidth = Some(Size(100, "px"))
    val expectedHeight = Some(Size(60, "px"))
    val result = root (p(Text("Some "),Image("",URI("picture.jpg", imgPath),width = expectedWidth,height = expectedHeight)))
    parse(input) should be (result)
  }
  
  
  "The replace directive" should "support regular inline markup" in {
    val input = """.. |subst| replace:: *text* here
      |
      |Some |subst|""".stripMargin
    val result = root (p(Text("Some "),SpanSequence(Emphasized("text"),Text(" here"))))
    parse(input) should be (result)
  }
  
  
  "The date directive" should "use the default pattern when no pattern is specified" in {
    val input = """.. |subst| date::
      |
      |Some |subst|""".stripMargin
    val date = PlatformDateFormat.format(new Date, "yyyy-MM-dd").toOption.get
    val result = root (p(Text(s"Some $date")))
    parse(input) should be (result)
  }
  
  it should "support custom patterns" in {
    val input = """.. |subst| date:: yyyy-MMM-dd
      |
      |Some |subst|""".stripMargin
    val date = PlatformDateFormat.format(new Date, "yyyy-MMM-dd").toOption.get
    val result = root (p(Text(s"Some $date")))
    parse(input) should be (result)
  }
  
  
  "The unicode directive" should "support unicode hex values starting with '0x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: 0xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values starting with 'x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values starting with '\\x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: \xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values starting with 'U+' intertwined with normal text" in {
    val input = """.. |subst| unicode:: U+A9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values starting with 'u' intertwined with normal text" in {
    val input = """.. |subst| unicode:: uA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values starting with '\\u' intertwined with normal text" in {
    val input = """.. |subst| unicode:: \""" + """uA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode hex values as XML-style entities intertwined with normal text" in {
    val input = """.. |subst| unicode:: &#xA9; Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  it should "support unicode decimal values intertwined with normal text" in {
    val input = """.. |subst| unicode:: 169 Company
      |
      |Copyright |subst|""".stripMargin
    val result = root (p(Text("Copyright " + '\u00a9'.toString + " Company")))
    parse(input) should be (result)
  }
  
  
}
