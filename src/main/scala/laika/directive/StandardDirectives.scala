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

package laika.directive

import Directives._
import laika.util.Builders._
import laika.tree.Elements._
import laika.tree.Templates._

/** 
 *  @author Jens Halm
 */
trait StandardDirectives {

  
  lazy val templateFor = Templates.create("for") {
    import Templates.Combinators._

    (attribute(Default) ~ body(Default) ~ context) {
      (path, content, context) => {
        context.resolveReference(path) match {
          case Some(value) => {
            val rules = laika.tree.Templates.rewriteRules(context.withReferenceContext(value))
            TemplateSpanSequence(content) rewrite rules
          }
          case None => TemplateString("")
        }
      }
    }
  }
  
  
  lazy val blockFragment = Blocks.create("fragment") {
    import Blocks.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => DocumentFragment(name, BlockSequence(content))
    }
  }
  
  // TODO - add template fragments
  
}