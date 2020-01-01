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

package laika.sbt

import java.util.concurrent.Executors

import cats.effect.{Blocker, ContextShift, IO}
import laika.api.{MarkupParser, Transformer}
import laika.api.builder.{BundleFilter, OperationConfig, ParserBuilder}
import laika.factory.MarkupFormat
import laika.format.{HTML, Markdown, PDF, ReStructuredText}
import laika.io.implicits._
import laika.io.model.DirectoryInput
import laika.io.runtime.DirectoryScanner
import laika.sbt.LaikaPlugin.autoImport._
import org.apache.fop.apps.FopFactory
import sbt.Keys.{artifact, artifactPath, excludeFilter, projectID, sourceDirectories, target}
import sbt._

import scala.concurrent.ExecutionContext

/** Implementations for Laika's sbt settings.
  *
  * @author Jens Halm
  */
object Settings {

  import Def._

  implicit lazy val processingContext: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  lazy val blocker: Blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))

  /** The FOP factory to use when producing PDF documents.
    */
  val fopFactory: Initialize[FopFactory] = setting {
    fopConfig.value map {
      FopFactory.newInstance
    } getOrElse PDF.defaultFopFactory
  }

  val describe: Initialize[String] = setting {

    val userConfig = laikaConfig.value

    def mergedConfig (config: OperationConfig): OperationConfig = {
      config.copy(
        bundleFilter = BundleFilter(strict = userConfig.strict, acceptRawContent = userConfig.rawContent),
        minMessageLevel = userConfig.renderMessageLevel
      )
    }

    def createParser (format: MarkupFormat): ParserBuilder = {
      val parser = MarkupParser.of(format)
      parser.withConfig(mergedConfig(parser.config)).using(laikaExtensions.value: _*)
    }

    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .withConfig(mergedConfig(createParser(Markdown).config))
      .using(laikaExtensions.value: _*)
      .io(blocker)
      .parallel[IO]
      .withAlternativeParser(createParser(ReStructuredText))
      .build

    val inputs = DirectoryScanner.scanDirectories[IO](DirectoryInput(
      (sourceDirectories in Laika).value,
      laikaConfig.value.encoding,
      transformer.config.docTypeMatcher,
      (excludeFilter in Laika).value.accept
    ))

    transformer
      .fromInput(inputs)
      .toDirectory((target in laikaSite).value)
      .describe
      .unsafeRunSync()
      .copy(renderer = "Depending on task")
      .formatted
  }


  /** The artifact path for the specified task.
    */
  def createArtifactPath (key: Scoped): Initialize[File] = setting {
    val art = (artifact in key).value
    val classifier = art.classifier map ("-"+_) getOrElse ""
    (target in Laika).value / (art.name + "-" + projectID.value.revision + classifier + "." + art.extension)
  }

  /** The set of targets for the transformation tasks of all supported output formats.
    */
  val allTargets = setting {
    Set(
      (target in laikaSite).value, 
      (artifactPath in laikaPDF).value, 
      (artifactPath in laikaEPUB).value, 
      (target in laikaXSLFO).value, 
      (target in laikaAST).value
    )
  }

}
