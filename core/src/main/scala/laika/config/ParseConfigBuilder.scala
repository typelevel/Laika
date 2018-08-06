/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.config

/** API for specifying configuration options that apply to all
  * kinds of operations that contain a parsing step (Parse and Transform APIs).
  *
  * @author Jens Halm
  */
trait ParseConfigBuilder extends OperationConfigBuilder {

  /**  Turns strict mode on for the target parser, switching off any
    *  features not part of the original markup syntax.
    *  This includes the registration of directives (custom tags), custom templates
    *  with directives, as well as configuration sections at the start of the document.
    *
    *  Technically it removes all `ExtensionBundle` instances which do not have
    *  the `useInStrictMode` flag set to true.
    */
  def strict: ThisType = withConfig(config.forStrictMode)

  /**  Enables all extensions that process raw content embedded into the host
    *  markup language.
    *  These are disabled by default as Laika is designed to render to multiple
    *  output formats from a single input document. With raw content embedded
    *  the markup document is tied to a specific output format.
    *
    *  Technically it activates all `ExtensionBundle` instances which have
    *  the `acceptRawContent` flag set to true.
    */
  def withRawContent: ThisType = withConfig(config.forRawContent)

}
