/*
 * Copyright 2015 the original author or authors.
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

package laika.render

import laika.tree.Documents.DocumentTree
import laika.factory.RenderResultProcessor
import laika.io.OutputProvider.OutputConfig
import laika.io.Output.BinaryOutput
import laika.api.Render
import java.io.ByteArrayOutputStream

class FOforPDFSpec {
  
  
  case class FOTest (config: PDFConfig) extends RenderResultProcessor[FOWriter] {
    
    val factory = XSLFO
    
    private val foForPDF = new FOforPDF(config)
    
    def process (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit, output: BinaryOutput) = {
    
      val fo = foForPDF.renderFO(tree, render)
      val out = output.asStream
      out.write(fo.getBytes("UTF-8"))
      
    }
    
  }
  
  
  trait Setup {
    
    def config: PDFConfig
    
    def tree: DocumentTree
    
    def render = {
      val stream = new ByteArrayOutputStream
      Render as FOTest(config) from tree toStream stream      
      stream.toString
    }
    
  }
  
  
}
