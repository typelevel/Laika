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

import laika.helium.builder.HeliumThemeBuilder
import laika.helium.config._
import laika.theme._

/**
  * @author Jens Halm
  */
class Helium private[laika] (private[laika] val siteSettings: SiteSettings,
                             private[laika] val epubSettings: EPUBSettings,
                             private[laika] val pdfSettings: PDFSettings) { self =>

  object site extends SiteOps {
    protected def helium: Helium = self
  }
  
  object epub extends EPUBOps {
    protected def helium: Helium = self
  }
  
  object pdf extends PDFOps {
    protected def helium: Helium = self
  }

  object all extends AllFormatsOps {
    protected def helium: Helium = self
  }
  
  def build: ThemeProvider = new HeliumThemeBuilder(this)
  
}

object Helium {
  
  val defaults: Helium = HeliumDefaults.instance
    
}
