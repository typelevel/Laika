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

package laika.helium

import laika.ast.Size

case class WebLayout (contentWidth: Size, 
                      navigationWidth: Size, 
                      defaultBlockSpacing: Size, 
                      defaultLineHeight: Double,
                      anchorPlacement: AnchorPlacement)

case class PDFLayout (pageWidth: Size, pageHeight: Size,
                      marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
                      defaultBlockSpacing: Size, defaultLineHeight: Double,
                      keepTogetherDecoratedLines: Int)

sealed trait AnchorPlacement

object AnchorPlacement {
  object None extends AnchorPlacement
  object Left extends AnchorPlacement
  object Right extends AnchorPlacement
}
