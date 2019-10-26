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

package laika.sbt

import laika.format.PDF
import laika.sbt.LaikaPlugin.autoImport._
import org.apache.fop.apps.FopFactory
import sbt.Keys.{artifact, artifactPath, projectID, target}
import sbt._

/** Implementations for Laika's sbt settings.
  *
  * @author Jens Halm
  */
object Settings {

  import Def._

  /** The FOP factory to use when producing PDF documents.
    */
  val fopFactory: Initialize[FopFactory] = setting {
    fopConfig.value map {
      FopFactory.newInstance
    } getOrElse PDF.defaultFopFactory
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
