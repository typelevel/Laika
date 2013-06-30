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

import laika.parse.rst.TextRoles.TextRole
import laika.parse.rst.TextRoles.Parts._
import laika.tree.Elements._

/** Defines all supported standard text roles of the reStructuredText reference parser.
 * 
 *  The following text roles are fully supported:
 * 
 *  - `emphasis`
 *  - `strong`
 *  - `literal`
 *  - `subscript` (and `sub` alias)
 *  - `superscript` (and `sup` alias)
 *  - `title-reference` (and `title` alias) - the default role
 *  - `raw` (+ format option) (needs to be enabled explicitly through `ReStructuredText.withRawElements`)
 *  
 *  The following text role is supported with some limitation:
 * 
 *  - `code` does currently not support syntax highlighting 
 *    (it allows to set the language so client-side highlighters can be integrated if required)
 *   
 *  The following text roles are not supported:
 * 
 *  - `math`
 *  - `pep-reference`
 *  - `rfc-reference`
 * 
 *  @author Jens Halm
 */
trait StandardTextRoles {
  
  private val classOption = optField("class", opt => Right(Options(None, opt.split(" ").toList))) map (_.getOrElse(NoOpt))
  
  lazy val emphasis: TextRole =
    TextRole("emphasis", NoOpt:Options)(classOption)((opt, text) => Emphasized(List(Text(text)), opt))
  
  lazy val strong: TextRole =
    TextRole("strong", NoOpt:Options)(classOption)((opt, text) => Strong(List(Text(text)), opt))

  lazy val literal: TextRole =
    TextRole("literal", NoOpt:Options)(classOption)((opt, text) => Literal(text, opt))  
    
  lazy val subscript: TextRole =
    TextRole("subscript", NoOpt:Options)(classOption)((opt, text) => Text(text, opt + Styles("subscript")))  
    
  lazy val superscript: TextRole =
    TextRole("superscript", NoOpt:Options)(classOption)((opt, text) => Text(text, opt + Styles("superscript")))
    
  lazy val sub: TextRole =
    TextRole("sub", NoOpt:Options)(classOption)((opt, text) => Text(text, opt + Styles("subscript"))) 
    
  lazy val sup: TextRole =
    TextRole("sup", NoOpt:Options)(classOption)((opt, text) => Text(text, opt + Styles("superscript")))
    
  lazy val titleRef: TextRole =
    TextRole("title-reference", NoOpt:Options)(classOption)((opt, text) => Emphasized(List(Text(text)), opt + Styles("title-reference")))
    
  lazy val title: TextRole =
    TextRole("title", NoOpt:Options)(classOption)((opt, text) => Emphasized(List(Text(text)), opt + Styles("title-reference")))
    
  lazy val codeSpan: TextRole =
    TextRole("code", ("",NoOpt:Options)) {
      (optField("language") ~ classOption) { (lang,opt) => (lang.getOrElse(""), opt) }
    }{
      case ((lang, opt), text) => Code(lang, List(Text(text)), opt)
    }  
    
  lazy val rawTextRole: TextRole =
    TextRole("raw", (Nil:List[String],NoOpt:Options)) {
      (field("format") ~ classOption) { (format,opt) => (format.split(" ").toList, opt) }
    }{
      case ((formats, opt), content) => RawContent(formats, content, opt)
    }  
    
  
  lazy val textRoles = List(emphasis,strong,literal,subscript,superscript,sub,sup,titleRef,title,codeSpan)
    
}