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

package laika.html

import java.io.{ StringReader, StringWriter }

import org.w3c.tidy.Tidy

/** @author Jens Halm
  */
object TidyHTML {

  def apply(html: String): String = {
    val in  = new StringReader(html)
    val out = new StringWriter
    val t   = new Tidy
    t.setTabsize(4)
    t.setPrintBodyOnly(true)
    t.setShowWarnings(false)
    t.setQuiet(true)
    t.setInputEncoding("UTF-8")
    t.setOutputEncoding("UTF-8")
    t.parse(in, out)
    out.toString
  }

}
