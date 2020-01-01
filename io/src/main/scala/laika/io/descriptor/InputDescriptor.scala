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

import cats.effect.Async
import cats.implicits._
import laika.ast.DocumentType
import laika.collection.TransitionalCollectionOps._
import laika.io.model.{BinaryInput, TextInput, TreeInput}

/** Describes a single, textual or binary input for a parsing or rendering operation.
  * This functionality is mostly intended for tooling support.
  * 
  * @author Jens Halm
  */
case class InputDescriptor (description: String, docType: DocumentType)

object InputDescriptor {

  def create[F[_]] (input: TextInput[F]): InputDescriptor = {
    val desc = input.sourceFile.fold(
      "In-memory string or stream"
    )(f => s"File '${f.getPath}'")
    apply(desc, input.docType)
  }

  def create[F[_]] (input: BinaryInput[F]): InputDescriptor = {
    val desc = input.sourceFile.fold(
      "In-memory bytes or stream"
    )(f => s"File '${f.getPath}'")
    apply(desc, DocumentType.Static)
  }
  
}

case class TreeInputDescriptor (inputs: Seq[InputDescriptor], sourceDirectories: Seq[String] = Nil) {
  
  def formatted: String = {
    val grouped = (inputs.map(in => (in.docType.toString, in.description)) ++ 
                    sourceDirectories.map(dir => ("Root Directories", dir)))
                    .groupBy(_._1).mapValuesStrict(_.map(_._2))
    
    TreeInputDescriptor.docTypeMappings.map { case (docType, typeDesc) =>
      val docs = grouped.get(docType).fold("-"){ _.mkString("\n    ") }
      s"""  $typeDesc
         |    $docs""".stripMargin  
    }.mkString("\n")
  }
  
}

object TreeInputDescriptor {
  
  private[descriptor] val docTypeMappings: Seq[(String, String)] = Seq(
    DocumentType.Markup.toString -> "Markup File(s)",
    DocumentType.Template.toString -> "Template(s)",
    DocumentType.Config.toString -> "Configuration Files(s)",
    DocumentType.StyleSheet.toString -> "CSS for PDF",
    DocumentType.Static.toString -> "Copied File(s)",
    "Root Directories" -> "Root Directories"
  )

  def create[F[_]: Async] (input: F[TreeInput[F]]): F[TreeInputDescriptor] = input.map { treeInput =>
    TreeInputDescriptor(
      treeInput.textInputs.map(InputDescriptor.create[F]) ++ treeInput.binaryInputs.map(InputDescriptor.create[F]), 
      treeInput.sourcePaths
    )
  }
  
}
