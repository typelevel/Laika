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

package laika.ast.sample

import laika.ast.Path.Root
import laika.ast._
import laika.parse.{ LineSource, SourceCursor, SourceFragment }

/** @author Jens Halm
  */
trait TestSourceBuilders {

  val defaultPath: Path = Root / "doc"

  def toSource(label: FootnoteLabel): String = label match {
    case FootnoteLabel.Autonumber             => "[#]_"
    case FootnoteLabel.Autosymbol             => "[*]_"
    case FootnoteLabel.AutonumberLabel(label) => s"[#$label]_"
    case FootnoteLabel.NumericLabel(label)    => s"[$label]_"
  }

  def source(fragment: String, root: String): SourceFragment = {
    val offset = root.indexOf(fragment)
    LineSource(fragment, SourceCursor(root).consume(offset))
  }

  def source(fragment: String, root: String, path: Path): SourceFragment = {
    val offset = root.indexOf(fragment)
    LineSource(fragment, SourceCursor(root, path).consume(offset))
  }

  def generatedSource(fragment: String): SourceFragment =
    LineSource(fragment, SourceCursor(fragment))

}
