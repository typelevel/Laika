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

package laika.parse.rst

import com.typesafe.config.ConfigValueFactory
import laika.parse.rst.Elements.FieldList
import laika.rewrite.TreeUtil
import laika.tree.Documents.Document
import laika.tree.Elements.{Comment, DecoratedHeader, Paragraph}

/**
  * @author Jens Halm
  */
object DocInfoExtractor extends (Document => Document) {

  def apply (doc: Document): Document = {
    import scala.collection.JavaConverters._

    val docStartBlock = doc.content.content.dropWhile {
      case _: Comment => true
      case _: DecoratedHeader => true
      case _ => false
    }.headOption

    val docInfoMap = docStartBlock.collect {
      case FieldList(fields, _) => fields.map { field =>
        val name = TreeUtil.extractText(field.name)
        val value = field.content.collect {
          case p: Paragraph => TreeUtil.extractText(p.content)
        }.mkString
        (name, value)
      }.toMap
    }

    val mergedConfig = docInfoMap.fold(doc.config){ info =>
      doc.config.withValue("docInfo", ConfigValueFactory.fromMap(info.asJava))
    }

    doc.copy(config = mergedConfig)
  }

}
