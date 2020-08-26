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

package laika.rewrite.nav

import cats.data.{Chain, NonEmptyChain}
import laika.ast.DocumentTreeRoot
import laika.config._

/**
  * @author Jens Halm
  */
case class Selections (selections: Seq[SelectionConfig]) {
  def getSelection (name: String): Option[SelectionConfig] = selections.find(_.name == name)
  def getClassifiers: Classifiers = Classifiers(selections.flatMap { group =>
    group.choices.find(_.selected).map(_.name)
  })
}
case class Classifiers (value: Seq[String])

object Selections {

  implicit val key: DefaultKey[Selections] = DefaultKey(LaikaKeys.selections)

  implicit val decoder: ConfigDecoder[Selections] = ConfigDecoder.seq[SelectionConfig].map(Selections.apply)

  implicit val encoder: ConfigEncoder[Selections] = ConfigEncoder.seq[SelectionConfig].contramap(_.selections)

  val empty = Selections(Nil)
  
  def apply (selection: SelectionConfig, selections: SelectionConfig*): Selections = Selections(selection +: selections)
  
  def createCombinationsConfig (config: Config): Seq[Seq[ChoiceConfig]] = {

    def createCombinations (value: Selections): Seq[Seq[ChoiceConfig]] =
      value.selections
        .filter(_.separateEbooks)
        .map(_.choices.toChain.toList.map(List(_)))
        .reduceLeftOption { (as, bs) =>
          for {a <- as; b <- bs} yield a ++ b
        }
        .getOrElse(Nil)

    config.get[Selections].fold(
      _ => Nil,
      choiceGroups => createCombinations(choiceGroups)
    )
  }
  
  def createCombinations (config: Config): NonEmptyChain[(Config, Classifiers)] = {
    
    val epubCoverImages = CoverImages.forEPUB(config)
    val pdfCoverImages = CoverImages.forPDF(config)
    
    def createCombinations(value: Selections): NonEmptyChain[Selections] = {
      val (separated, nonSeparated) = value.selections.partition(_.separateEbooks)
      
      val combinations = separated
        .map { group =>
          group.choices.map(choice => NonEmptyChain.one(group.select(choice)))
        }
        .reduceLeftOption { (as, bs) =>
          for {a <- as; b <- bs} yield a ++ b
        }

      combinations.fold(NonEmptyChain.one(value))(_.map {
        combined => Selections(combined.toChain.toList ++ nonSeparated)
      })
    }
    
    def addCoverImages (value: Selections, config: ConfigBuilder): ConfigBuilder = {
      val classifier = value.getClassifiers.value.mkString("-")
      val withEPUB = epubCoverImages.getImageFor(classifier).fold(config) { img =>
        config.withValue(LaikaKeys.root.child("epub").child(LaikaKeys.coverImage.local), img)
      }
      pdfCoverImages.getImageFor(classifier).fold(withEPUB) { img =>
        withEPUB.withValue(LaikaKeys.root.child("pdf").child(LaikaKeys.coverImage.local), img)
      }
    }
    
    config.get[Selections].fold(
      _ => NonEmptyChain.one((config, Classifiers(Nil))),
      choiceGroups => createCombinations(choiceGroups).map { newConfig =>
        val populatedConfig = addCoverImages(newConfig, config.withValue(newConfig)).build
        (populatedConfig, newConfig.getClassifiers)
      }
    )
  }

  def createCombinations (root: DocumentTreeRoot): NonEmptyChain[(DocumentTreeRoot, Classifiers)] = {
    createCombinations(root.config).map { case (config, classifiers) => (root.withConfig(config), classifiers) }
  }
  
}

case class SelectionConfig (name: String, choices: NonEmptyChain[ChoiceConfig], separateEbooks: Boolean = false) {
  def withSeparateEbooks: SelectionConfig = copy(separateEbooks = true)
  def getLabel (name: String): Option[String] = choices.find(_.name == name).map(_.label)
  def select (choice: ChoiceConfig): SelectionConfig = 
    copy(choices = choices.map(c => if (c == choice) c.copy(selected = true) else c))
}

object SelectionConfig {
  
  def apply (name: String, choice: ChoiceConfig, choices: ChoiceConfig*): SelectionConfig =
    SelectionConfig(name, NonEmptyChain.fromChainPrepend(choice, Chain.fromSeq(choices)))
  
  implicit val decoder: ConfigDecoder[SelectionConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name     <- config.get[String]("name")
      choices  <- config.get[NonEmptyChain[ChoiceConfig]]("choices")
      separate <- config.get[Boolean]("separateEbooks", false)
    } yield {
      SelectionConfig(name, choices, separate)
    }
  }
  implicit val encoder: ConfigEncoder[SelectionConfig] = ConfigEncoder[SelectionConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("name", config.name)
      .withValue("choices", config.choices)
      .withValue("separateEbooks", config.separateEbooks)
      .build
  }
}

case class ChoiceConfig (name: String, label: String, selected: Boolean = false)

object ChoiceConfig {
  implicit val decoder: ConfigDecoder[ChoiceConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name  <- config.get[String]("name")
      label <- config.get[String]("label")
      selected <- config.get[Boolean]("selected", false)
    } yield {
      ChoiceConfig(name, label, selected)
    }
  }
  implicit val encoder: ConfigEncoder[ChoiceConfig] = ConfigEncoder[ChoiceConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("name", config.name)
      .withValue("label", config.label)
      .withValue("selected", config.selected)
      .build
  }
}
