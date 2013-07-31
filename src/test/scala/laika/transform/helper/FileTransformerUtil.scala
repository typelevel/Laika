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

package laika.transform.helper

import java.io.StringReader
import java.io.StringWriter

import org.w3c.tidy.Tidy

/** Helpers for tests that read entire sample files with markup and compare
 *  them to pre-rendered reference files containing the expected HTML.
 * 
 * @author Jens Halm
 */
trait FileTransformerUtil {

  def classPathResource (path: String) = getClass.getResource(path).getFile
  
  def readFile (name: String) = {
    val source = scala.io.Source.fromFile(name)
    val lines = source.mkString
    source.close()
    lines
  }
  
  def tidy (html: String) = {
    val in = new StringReader(html)
    val out = new StringWriter
    val t = new Tidy
    t.setTabsize(4)
    t.setPrintBodyOnly(true)
    t.setShowWarnings(false)
    t.setQuiet(true)
    t.parse(in, out)
    out.toString
  }
  
  
}