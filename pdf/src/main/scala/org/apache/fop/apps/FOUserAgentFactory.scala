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

package org.apache.fop.apps

import org.apache.xmlgraphics.io.ResourceResolver
import org.apache.fop.apps.io.ResourceResolverFactory
import java.net.URI

/** An adapter class that is necessary as Apache FOP does not
  *  allow for a ResourceResolver to get specified per Fop instance
  *  in its public API. This is required in Laika's PDF support
  *  as the URIs get resolved relative to all source paths
  *  of the markup parsers, which can vary between invocations.
  *
  *  @author Jens Halm
  */
object FOUserAgentFactory {

  /** Creates a new FOUserAgent with the specified factory
    *  and resolver.
    *
    *  @param fopFactory the factory the new user agent should use
    *  @param resourceResolver a custom resource resolver for the new user agent
    */
  def createFOUserAgent(fopFactory: FopFactory, resourceResolver: ResourceResolver): FOUserAgent = {
    val resolver =
      ResourceResolverFactory.createInternalResourceResolver(new URI("file:///"), resourceResolver)
    new FOUserAgent(fopFactory, resolver)
  }

}
