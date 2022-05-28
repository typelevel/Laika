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

package laika.io.descriptor

import cats.effect.Sync
import cats.implicits._
import laika.ast.DocumentType
import laika.collection.TransitionalCollectionOps._
import laika.io.model.{BinaryInput, FilePath, InputTree, TextInput}

/** Describes a single, textual or binary input for a parsing or rendering operation.
  * This functionality is mostly intended for tooling support.
  * 
  * @author Jens Halm
  */
case class InputDescriptor (description: String, docType: DocumentType)

object InputDescriptor {

  def create[F[_]] (input: TextInput[F]): InputDescriptor = {
    val desc = input.sourceFile.fold(
      s"${input.path.toString}: in-memory string or stream"
    )(f => s"${input.path.toString}: file '${f.toString}'")
    apply(desc, input.docType)
  }

  def create[F[_]] (input: BinaryInput[F]): InputDescriptor = {
    val desc = input.sourceFile.fold(
      s"${input.path.toString}: in-memory bytes or stream"
    )(f => s"${input.path.toString}: file '${f.toString}'")
    apply(desc, DocumentType.Static())
  }
  
}

case class TreeInputDescriptor (inputs: Seq[InputDescriptor], sourceDirectories: Seq[FilePath] = Nil, missingDirectories: Seq[FilePath] = Nil) {
  
  def formatted: String = {
    val descriptions = inputs.map(in => (in.docType.productPrefix, in.description)) ++
      sourceDirectories.map(dir => (TreeInputDescriptor.rootDirectoriesText, dir.toString)) ++ 
      missingDirectories.map(dir => (TreeInputDescriptor.rootDirectoriesText, dir.toString + " (does not exist or is not a directory)"))
    
    val grouped = descriptions.groupBy(_._1).mapValuesStrict(_.map(_._2))
    
    TreeInputDescriptor.docTypeMappings.map { case (docType, typeDesc) =>
      val docs = grouped.get(docType).fold("-"){ _.distinct.sorted.mkString("\n    ") }
      s"""  $typeDesc
         |    $docs""".stripMargin  
    }.mkString("\n").drop(2)
  }
  
}

object TreeInputDescriptor {

  private[descriptor] val rootDirectoriesText = "Root Directories"
  
  private[descriptor] val docTypeMappings: Seq[(String, String)] = Seq(
    DocumentType.Markup.productPrefix -> "Markup File(s)",
    DocumentType.Template.productPrefix -> "Template(s)",
    DocumentType.Config.productPrefix -> "Configuration Files(s)",
    DocumentType.StyleSheet("").productPrefix -> "CSS for PDF",
    DocumentType.Static().productPrefix -> "Copied File(s)",
    rootDirectoriesText -> rootDirectoriesText
  )

  def create[F[_]] (input: InputTree[F], missingDirectories: Seq[FilePath]): TreeInputDescriptor = 
    TreeInputDescriptor(
      input.textInputs.map(InputDescriptor.create[F]) ++ input.binaryInputs.map(InputDescriptor.create[F]),
      input.sourcePaths,
      missingDirectories
    )
  
}
