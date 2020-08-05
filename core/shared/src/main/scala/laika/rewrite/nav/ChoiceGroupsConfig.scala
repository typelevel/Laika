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

import cats.data.NonEmptyChain
import laika.ast.{DocumentTreeRoot, Path}
import laika.config.{Config, ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

/**
  * @author Jens Halm
  */
case class ChoiceGroupsConfig (choices: Seq[ChoiceGroupConfig]) {
  def getGroupConfig (name: String): Option[ChoiceGroupConfig] = choices.find(_.name == name)
  def getClassifiers: Classifiers = Classifiers(choices.flatMap { group =>
    group.choices.find(_.selected).map(_.name)
  })
}
case class Classifiers (value: Seq[String])

object ChoiceGroupsConfig {

  implicit val key: DefaultKey[ChoiceGroupsConfig] = DefaultKey(LaikaKeys.choices)

  implicit val decoder: ConfigDecoder[ChoiceGroupsConfig] = ConfigDecoder.seq[ChoiceGroupConfig].map(ChoiceGroupsConfig.apply)

  implicit val encoder: ConfigEncoder[ChoiceGroupsConfig] = ConfigEncoder.seq[ChoiceGroupConfig].contramap(_.choices)

  def createChoiceCombinationsConfig (config: Config): Seq[Seq[ChoiceConfig]] = {

    def createCombinations (value: ChoiceGroupsConfig): Seq[Seq[ChoiceConfig]] =
      value.choices
        .filter(_.separateEbooks)
        .map(_.choices.toChain.toList.map(List(_)))
        .reduceLeftOption { (as, bs) =>
          for {a <- as; b <- bs} yield a ++ b
        }
        .getOrElse(Nil)

    config.get[ChoiceGroupsConfig].fold(
      _ => Nil,
      choiceGroups => createCombinations(choiceGroups)
    )
  }
  
  def createChoiceCombinations (config: Config): NonEmptyChain[(Config, Classifiers)] = {
    
    def createCombinations(value: ChoiceGroupsConfig): NonEmptyChain[ChoiceGroupsConfig] = {
      val (separated, nonSeparated) = value.choices.partition(_.separateEbooks)
      
      val combinations = separated
        .map { group =>
          group.choices.map(choice => NonEmptyChain.one(group.select(choice)))
        }
        .reduceLeftOption { (as, bs) =>
          for {a <- as; b <- bs} yield a ++ b
        }

      combinations.fold(NonEmptyChain.one(value))(_.map {
        combined => ChoiceGroupsConfig(combined.toChain.toList ++ nonSeparated)
      })
    }
    
    config.get[ChoiceGroupsConfig].fold(
      _ => NonEmptyChain.one((config, Classifiers(Nil))),
      choiceGroups => createCombinations(choiceGroups).map { newConfig =>
        (config.withValue(newConfig).build, newConfig.getClassifiers)
      }
    )
  }

  def createChoiceCombinations (root: DocumentTreeRoot): NonEmptyChain[(DocumentTreeRoot, Classifiers)] = {
    createChoiceCombinations(root.config).map { case (config, classifiers) => (root.withConfig(config), classifiers) }
  }
  
}

case class ChoiceGroupConfig (name: String, choices: NonEmptyChain[ChoiceConfig], separateEbooks: Boolean = false) {
  def getLabel (name: String): Option[String] = choices.find(_.name == name).map(_.label)
  def select (choice: ChoiceConfig): ChoiceGroupConfig = 
    copy(choices = choices.map(c => if (c == choice) c.copy(selected = true) else c))
}

object ChoiceGroupConfig {
  implicit val decoder: ConfigDecoder[ChoiceGroupConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name     <- config.get[String]("name")
      choices  <- config.get[NonEmptyChain[ChoiceConfig]]("choices")
      separate <- config.get[Boolean]("separateEbooks", false)
    } yield {
      ChoiceGroupConfig(name, choices, separate)
    }
  }
  implicit val encoder: ConfigEncoder[ChoiceGroupConfig] = ConfigEncoder[ChoiceGroupConfig] { config =>
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
