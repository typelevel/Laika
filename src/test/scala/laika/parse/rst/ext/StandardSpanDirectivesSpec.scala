/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst.ext

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.parse.rst.ReStructuredText
import laika.api.Parse
import java.text.SimpleDateFormat
import java.util.Date

/**
 * @author Jens Halm
 */
class StandardSpanDirectivesSpec extends FlatSpec 
                                  with ShouldMatchers 
                                  with ModelBuilder {

  
  "The image directive" should "parse the URI argument" in {
    val input = """.. |subst| image:: picture.jpg
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),img("", "picture.jpg")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the alt option" in {
    val input = """.. |subst| image:: picture.jpg
      | :alt: alt
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),img("alt", "picture.jpg")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the target option with a simple reference" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: ref_
      |
      |.. _ref: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the target option with a phrase reference" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: `some ref`_
      |
      |.. _`some ref`: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the target option with a uri" in {
    val input = """.. |subst| image:: picture.jpg
      | :target: http://foo.com/
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),ExternalLink(List(img("", "picture.jpg")), "http://foo.com/")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the class option" in {
    val input = """.. |subst| image:: picture.jpg
      | :class: foo
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),Image("","picture.jpg",options=Styles("foo"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The replace directive" should "support regular inline markup" in {
    val input = """.. |subst| replace:: *text* here
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),SpanSequence(List(em("text"),txt(" here")))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The date directive" should "use the default pattern when no pattern is specified" in {
    val input = """.. |subst| date::
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),txt((new SimpleDateFormat("yyyy-MM-dd").format(new Date)))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support custom patterns" in {
    val input = """.. |subst| date:: yyyy-MMM-dd
      |
      |Some |subst|""".stripMargin
    val result = doc (p(txt("Some "),txt((new SimpleDateFormat("yyyy-MMM-dd").format(new Date)))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The unicode directive" should "support unicode hex values starting with '0x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: 0xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values starting with 'x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values starting with '\\x' intertwined with normal text" in {
    val input = """.. |subst| unicode:: \xA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values starting with 'U+' intertwined with normal text" in {
    val input = """.. |subst| unicode:: U+A9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values starting with 'u' intertwined with normal text" in {
    val input = """.. |subst| unicode:: uA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values starting with '\\u' intertwined with normal text" in {
    val input = """.. |subst| unicode:: \""" + """uA9 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode hex values as XML-style entities intertwined with normal text" in {
    val input = """.. |subst| unicode:: &#xA9; Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support unicode decimal values intertwined with normal text" in {
    val input = """.. |subst| unicode:: 169 Company
      |
      |Copyright |subst|""".stripMargin
    val result = doc (p(txt("Copyright "),txt('\u00a9'.toString+" Company")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
}