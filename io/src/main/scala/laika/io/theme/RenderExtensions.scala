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

package laika.io.theme

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentDocument
import laika.ast.{Element, Header, InternalTarget, SpanLink, SpanSequence, Styles}
import laika.render.HTMLFormatter

/**
  * @author Jens Halm
  */
object RenderExtensions {

  val value: PartialFunction[(HTMLFormatter, Element), String] = {
    case (fmt, Header(level, content, opt)) =>
      val link = opt.id.map(id => SpanLink(Seq(SpanSequence(Nil, Styles("anchor"))), InternalTarget(Root, CurrentDocument(id)), options = Styles("anchor-link")))
      fmt.newLine + fmt.element("h"+level.toString, opt, link.toSeq ++ content)
  }
  
}
