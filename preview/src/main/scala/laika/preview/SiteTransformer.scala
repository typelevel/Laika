/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.preview

import java.io.InputStream

import cats.syntax.all._
import cats.effect.{Async, Resource}
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.Path
import laika.format.HTML
import laika.io.api.{TreeParser, TreeRenderer}
import laika.io.implicits._
import laika.io.model.{InputTreeBuilder, StringTreeOutput}
import laika.theme.ThemeProvider

class SiteTransformer[F[_]: Async] (val parser: TreeParser[F], htmlRenderer: TreeRenderer[F], inputs: InputTreeBuilder[F]) {

  private val tree = {
    //        val apiPath = validated(SiteConfig.apiPath(baseConfig))
    //        val inputs = generateAPI.value.foldLeft(laikaInputs.value.delegate) {
    //          (inputs, path) => inputs.addProvidedPath(apiPath / path)
    //        }
    parser.fromInput(inputs).parse
  }

  val transform: F[Map[Path, Either[Resource[F, InputStream], String]]] = tree.flatMap { tree =>
    htmlRenderer
      .from(tree.root)
      .copying(tree.staticDocuments)
      .toOutput(StringTreeOutput)
      .render
      .map { root =>
        (root.allDocuments.map { doc =>
          (doc.path, Right(doc.content))
        } ++
          root.staticDocuments.map { doc =>
            (doc.path, Left(doc.input))
          })
          .toMap // TODO - map root
      }
  }
  
}

object SiteTransformer {

  def htmlRenderer[F[_]: Async] (config: OperationConfig, theme: ThemeProvider): Resource[F, TreeRenderer[F]] = Renderer
    .of(HTML)
    .withConfig(config)
    .parallel[F]
    .withTheme(theme)
    .build
  
  def create[F[_]: Async](parser: Resource[F, TreeParser[F]],
                          inputs: InputTreeBuilder[F],
                          theme: ThemeProvider): Resource[F, SiteTransformer[F]] = {
    for {
      p   <- parser
      r   <- htmlRenderer(p.config, theme)
    } yield new SiteTransformer[F](p,r, inputs)
    
  }
  
}
