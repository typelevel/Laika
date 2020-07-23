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

import laika.config.{Config, ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

/**
  * @author Jens Halm
  */
case class ChoiceGroupsConfig (choices: Seq[ChoiceGroupConfig]) {
  def getGroupConfig (name: String): Option[ChoiceGroupConfig] = choices.find(_.name == name)
}

object ChoiceGroupsConfig {

  implicit val key: DefaultKey[ChoiceGroupsConfig] = DefaultKey(LaikaKeys.choices)

  implicit val decoder: ConfigDecoder[ChoiceGroupsConfig] = ConfigDecoder.seq[ChoiceGroupConfig].map(ChoiceGroupsConfig.apply)

  implicit val encoder: ConfigEncoder[ChoiceGroupsConfig] = ConfigEncoder.seq[ChoiceGroupConfig].contramap(_.choices)
  
  def createChoiceCombinations (config: Config): Seq[Config] = {
    
    def createCombinations(value: ChoiceGroupsConfig): Seq[ChoiceGroupsConfig] = {
      val (separated, nonSeparated) = value.choices.partition(_.separateEbooks)
      
      val variations: Seq[Seq[ChoiceGroupConfig]] = separated.map { group =>
        group.choices.indices.map { index =>
          group.copy(choices = group.choices.updated(index, group.choices(index).copy(selected = true)))
        }
      }

      val combinations = variations
        .map(_.map(Seq(_)))
        .reduceLeftOption { (as, bs) =>
          for {a <- as; b <- bs} yield a ++ b
        }
      
      combinations.fold(Seq(value))(_.map(combined => ChoiceGroupsConfig(combined ++ nonSeparated)))
    }
    
    config.get[ChoiceGroupsConfig].fold(
      _ => Seq(config),
      choiceGroups => createCombinations(choiceGroups).map { newConfig =>
        config.withValue(newConfig).build
      }
    )
  }
  
}

case class ChoiceGroupConfig (name: String, choices: Seq[ChoiceConfig], separateEbooks: Boolean = false) {
  def getLabel (name: String): Option[String] = choices.find(_.name == name).map(_.label)
}

object ChoiceGroupConfig {
  implicit val decoder: ConfigDecoder[ChoiceGroupConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name     <- config.get[String]("name")
      choices  <- config.get[Seq[ChoiceConfig]]("choices")
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
