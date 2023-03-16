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

package laika.render.epub

/** Holds static content for EPUB metadata files that do not depend on the input.
  *
  * @author Jens Halm
  */
object StaticContent {

  /** The content of the `mimetype` file in the EPUB root directory.
    */
  val mimeType = "application/epub+zip"

  /** The content of the file `container.xml` in the `META-INF` folder of the EPUB container.
    */
  val container =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
      |  <rootfiles>
      |    <rootfile full-path="EPUB/content.opf" media-type="application/oebps-package+xml" />
      |  </rootfiles>
      |</container>""".stripMargin

  /** iBook configuration options.
    */
  val iBooksOptions = """<?xml version="1.0" encoding="UTF-8"?>
                        |<display_options>
                        |  <platform name="*">
                        |    <option name="specified-fonts">true</option>
                        |  </platform>
                        |</display_options>""".stripMargin

}
