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

import laika.config.{ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

/**
  * @author Jens Halm
  */
case class ChoiceGroupsConfig (choices: Seq[ChoiceGroupConfig]) {
  def getGroupConfig (name: String): Option[ChoiceGroupConfig] = ???
}

object ChoiceGroupsConfig {

  implicit val key: DefaultKey[ChoiceGroupsConfig] = DefaultKey(LaikaKeys.choices)

  implicit val decoder: ConfigDecoder[ChoiceGroupsConfig] = ConfigDecoder.seq[ChoiceGroupConfig].map(ChoiceGroupsConfig.apply)

  implicit val encoder: ConfigEncoder[ChoiceGroupsConfig] = ConfigEncoder.seq[ChoiceGroupConfig].contramap(_.choices)
  
}

case class ChoiceGroupConfig (name: String, choices: Seq[ChoiceConfig]) {
  def getLabel (name: String): Option[String] = ???
}

object ChoiceGroupConfig {
  implicit val decoder: ConfigDecoder[ChoiceGroupConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name    <- config.get[String]("name")
      choices <- config.get[Seq[ChoiceConfig]]("choices")
    } yield {
      ChoiceGroupConfig(name, choices)
    }
  }
  implicit val encoder: ConfigEncoder[ChoiceGroupConfig] = ConfigEncoder[ChoiceGroupConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("name", config.name)
      .withValue("choices", config.choices)
      .build
  }
}

case class ChoiceConfig (name: String, label: String)

object ChoiceConfig {
  implicit val decoder: ConfigDecoder[ChoiceConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name  <- config.get[String]("name")
      label <- config.get[String]("label")
    } yield {
      ChoiceConfig(name, label)
    }
  }
  implicit val encoder: ConfigEncoder[ChoiceConfig] = ConfigEncoder[ChoiceConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("name", config.name)
      .withValue("label", config.label)
      .build
  }
}
