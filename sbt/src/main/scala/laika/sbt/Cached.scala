/*
 * Copyright 2014 the original author or authors.
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

import java.io.File
import sbt.Difference
import sbt.Path
import sbt.ChangeReport
import sbt.FilesInfo
import LaikaSbtPlugin.OutputFormats.OutputFormat

/** Modified version of sbt's `FileFunction` object that allows for a more fine grained change detection.
 *  In the original sbt implementation one set of input files was always mapped to one set of output files
 *  to determine if any of those have changed and the associated action should be run.
 *  
 *  In Laika's `generate` task the output formats can be specified dynamically on the command line and
 *  only the sets of output files that match the command should be considered when performing the change
 *  detection. The command `generate pdf html` for example should only look for changes in the input files
 *  and whether the output files in the targets for PDF and HTML are still present.
 *  
 *  This means that we have to map one set of input files to a variable number of sets of output files.
 *  If the input files have been modified, all output files will need to be regenerated. If only one set
 *  of output files has been deleted, only these will be regenerated.
 * 
 *  @author Jens Halm
 */
class Cached (cacheBaseDirectory: File) {
  
  val inStyle = FilesInfo.lastModified
  val outStyle = FilesInfo.exists
  
  /** Always performs the specified action, independent of the content of
   *  the change report. The latter will instead be used to pass it to 
   *  separate output actions.
   */
  def inputs (action: ChangeReport[File] => Set[File]): Set[File] => Set[File] = {
    import Path._
    lazy val inCache = Difference.inputs(cacheBaseDirectory / "in-cache", inStyle)
    inputs => inCache(inputs)(action)
  }

  /** Performs the specified action when either the input report that is passed to this
   *  function or the output report for the specified format contains modified files.
   */
  def outputs (inReport: ChangeReport[File], format: OutputFormat)(action: => Set[File]): Set[File] = {
    import Path._
    lazy val outCache = Difference.outputs(cacheBaseDirectory / s"$format-cache", outStyle)
    outCache { outReport =>
      if (inReport.modified.isEmpty && outReport.modified.isEmpty)
        outReport.checked
      else
        action
    }
  }
  
}
