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

package laika.render.pdf

import java.io.OutputStream
import java.net.URI

import cats.syntax.all._
import cats.effect.Async
import cats.effect.std.Dispatcher
import laika.ast.Path
import laika.io.model.BinaryInput
import org.apache.fop.apps.io.ResourceResolverFactory
import org.apache.xmlgraphics.io.{Resource, ResourceResolver}

/** Adapter for the ResourceResolver API of Apache FOP, allowing Laika to serve any kind of InputStream
  * from its static input documents, including in-memory content or classpath resources, 
  * for which FOP does not have any support built in.
  * 
  * The synchronous, blocking, and non-RT APIs of Apache FOP require the use of a cats-effect `Dispatcher`.
  * 
  * Note that this involves a small risk of resources leaking as Laika has to rely on Apache FOP for
  * closing the InputStream, there is no way to know when it would be safe to do that within Laika.
  * 
  * @author Jens Halm
  */
class FopResourceResolver[F[_]: Async] (input: Seq[BinaryInput[F]], dispatcher: Dispatcher[F]) extends ResourceResolver {

  private val fallbackResolver = ResourceResolverFactory.createDefaultResourceResolver()
  
  private val resourceMap = input.map(s => (s.path, s.input)).toMap

  def getResource (uri: URI): Resource =
    if (uri.isAbsolute && uri.getScheme != "file") fallbackResolver.getResource(uri)
    else resourceMap.get(Path.parse(uri.getPath)).fold(fallbackResolver.getResource(uri))(in => 
      new Resource(dispatcher.unsafeRunSync(fs2.io.toInputStreamResource(in).allocated.map(_._1)))
    )

  def getOutputStream (uri: URI): OutputStream = fallbackResolver.getOutputStream(uri)

}
