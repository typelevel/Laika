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

/** Groups configuration for multiple @:select directives.
  * 
  * The `@:select` directive is a special directive that allows to create alternative versions of the same documentation,
  * for example one with Scala code examples and one with Java. 
  * Or as in the case of Laika's own documentation, one showing configuration setup with sbt and the other 
  * through the library API.
  *
  * When using the default Helium theme these alternatives will be rendered as tabs in the website.
  *
  * While for EPUB and PDF output it triggers the generation of separate books for each of the alternatives
  * and offers them on the download page to cater for the fact that tabs do not work well on paper.
  * 
  * @author Jens Halm
  */
case class Selections (selections: Seq[SelectionConfig]) {

  /** Returns the selection with the specified name, if present.
    */
  def getSelection (name: String): Option[SelectionConfig] = selections.find(_.name == name)

  /** Returns all classifiers of the configured selections based on their selection status.
    * 
    * This is a rather low-level method that you'd only need to use if you want to replicate
    * the functionality that Laika's sbt plugin offers around producing different variants of the same
    * documentation, based on the use of `@:select` directives in text markup.
    * 
    * For example, if you have one selection that offers code samples in Java or Scala,
    * and another that offers build examples for sbt or Maven, then this method would 
    * return `Seq("scala", "sbt")` if `scala` and `sbt` are the selected choices in their respective groups
    * in the current render operation.
    * 
    * The classifier is usually added to the artifact base name when Laika produces EPUB and PDF
    * output for different selections.
    * In the setup above, you would have four different EPUB files, `[basename]-scala-maven.epub`, 
    * `[basename]-java-maven.epub`, `[basename]-scala-sbt.epub` and `[basename]-java-sbt.epub` and 
    * also four PDF files with corresponding names.
    */
  def getClassifiers: Classifiers = Classifiers(selections.flatMap { group =>
    group.choices.find(_.selected).map(_.name)
  })
}

case class Classifiers (value: Seq[String])

/** Companion for creating selection config instances that can be passed to Laika configuration
  * and builders that produce configuration of all possible combinations of @:select directives.
  * 
  * See the documentation for the `@:select` directive in the manual for the full context of this feature.
  */
object Selections {

  implicit val key: DefaultKey[Selections] = DefaultKey(LaikaKeys.selections)

  implicit val decoder: ConfigDecoder[Selections] = ConfigDecoder.seq[SelectionConfig].map(Selections.apply)

  implicit val encoder: ConfigEncoder[Selections] = ConfigEncoder.seq[SelectionConfig].contramap(_.selections)

  val empty = Selections(Nil)

  /** Groups configuration for one or more @:select directives in an instance that can be passed to Laika configuration.
    * 
    * Example for Laika's own manual, which allows the user to chose between seeing configuration examples
    * for sbt or for the library API:
    * 
    * {{{
    * laikaConfig := LaikaConfig.defaults
    *   .withConfigValue(Selections(
    *     SelectionConfig("config",
    *       ChoiceConfig("sbt", "sbt Plugin"),
    *       ChoiceConfig("library", "Library API")
    *     ).withSeparateEbooks
    *   ))
    * }}}
    * 
    * See the documentation for the `@:select` directive in the manual for the full context of this feature.
    */
  def apply (selection: SelectionConfig, selections: SelectionConfig*): Selections = Selections(selection +: selections)

  /** Creates all valid combinations of choices for the given configuration instance.
    * 
    * This is a rather low-level method that you'd only need to use if you want to replicate
    * the functionality that Laika's sbt plugin offers around producing different variants of the same
    * documentation, based on the use of `@:select` directives in text markup.
    * 
    * See the documentation for the `@:select` directive in the manual in this case for the full context
    * of this feature.
    * 
    * In contrast to the `createCombinations` method this one is mostly useful for features like generating
    * a download page for all e-book artifacts produced by the renderer as it also contains the label information.
    */
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

  private[laika] def createCombinations (config: Config): NonEmptyChain[(Config, Classifiers)] = {
    
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
    
    def populateConfig (value: Selections, configBuilder: ConfigBuilder): ConfigBuilder = {
      val classifier = value.getClassifiers.value.mkString("-")
      val withEPUB = epubCoverImages.getImageFor(classifier).fold(configBuilder) { img =>
        configBuilder.withValue(LaikaKeys.root.child("epub").child(LaikaKeys.coverImage.local), img)
      }
      val withImages = pdfCoverImages.getImageFor(classifier).fold(withEPUB) { img =>
        withEPUB.withValue(LaikaKeys.root.child("pdf").child(LaikaKeys.coverImage.local), img)
      }
      if (classifier.isEmpty) withImages else {
        val baseKey = "metadata.identifier"
        val fallback = config.get[String](baseKey).toOption
        val epubId = config.get[String](s"epub.$baseKey").toOption.orElse(fallback).map(_ + classifier)
        val pdfId = config.get[String](s"pdf.$baseKey").toOption.orElse(fallback).map(_ + classifier)
        withImages
          .withValue(s"epub.$baseKey", epubId)
          .withValue(s"pdf.$baseKey", pdfId)
      }
    }
    
    config.get[Selections].fold(
      _ => NonEmptyChain.one((config, Classifiers(Nil))),
      choiceGroups => createCombinations(choiceGroups).map { newConfig =>
        val populatedConfig = populateConfig(newConfig, config.withValue(newConfig)).build
        (populatedConfig, newConfig.getClassifiers)
      }
    )
  }

  /** Creates all valid combinations of choices for the given configuration instance.
    * The returned chain contains tuples that contain both, the modified document tree that can be passed
    * to EPUB or PDF renderers to produce a specific version of the document tree only containing
    * one set of selections and the classifiers that can be appended to the artifact name for distinction.
    * 
    * Concretely this means that you pass in a single document tree obtained from a parser
    * that might contain one or more `@:select` directives for alternate content the user can choose from,
    * and get back several new document trees where in each of them all but one of the choices have been
    * removed from every `@:select` directive.
    * This is useful for e-book rendering where it is not desirable or possible to show the alternative
    * content in interactive tabs.
    * It is therefore used for EPUB and PDF rendering, but not for site generation.
    *
    * This is a rather low-level method that you'd only need to use if you want to replicate
    * the functionality that Laika's sbt plugin offers around producing different variants of the same
    * documentation, based on the use of `@:select` directives in text markup.
    *
    * See the documentation for the `@:select` directive in the manual in this case for the full context
    * of this feature.
    */
  def createCombinations (root: DocumentTreeRoot): NonEmptyChain[(DocumentTreeRoot, Classifiers)] = {
    createCombinations(root.config).map { case (config, classifiers) => (root.withConfig(config), classifiers) }
  }
  
}

/** Configuration for a single kind of selection and its choices available for the user.
  * 
  * @param name the name of the selection as used in text markup, e.g. `@:select(name)`.
  * @param choices the configuration for one or more choices that are available in each of the directives
  * @param separateEbooks whether the selection should render all its choices in the same output or produce
  *                       separate e-books where in each of them only one of the choice is displayed.
  *                       This way separate e-books for Scala vs. Java code samples or sbt vs. Maven build
  *                       examples can be produced.
  *                       Keep in mind that multiple selections having this property set to true would result 
  *                       in the cartesian product of available e-book versions,
  *                       it is therefore unusual to have more than one or two.
  */
case class SelectionConfig (name: String, choices: NonEmptyChain[ChoiceConfig], separateEbooks: Boolean = false) {

  /** Specifies that the choices of this selection should be rendered in entirely separate e-books
    * and not below each other in the same output. 
    */
  def withSeparateEbooks: SelectionConfig = copy(separateEbooks = true)

  /** Returns the label of the choice with the specified name, if present.
    */
  def getLabel (name: String): Option[String] = choices.find(_.name == name).map(_.label)

  /** Returns a copy of this instance where the specified choice is marked as selected.
    */
  def select (choice: ChoiceConfig): SelectionConfig = 
    copy(choices = choices.map(c => if (c == choice) c.copy(selected = true) else c))
}

object SelectionConfig {

  /** Creates a new configuration instance for a single kind of selection and its choices available for the user.
    */
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

/** Configuration for a single choice within a selection.
  * 
  * @param name the name of the selection as used in text markup, e.g. `@:choice(name)`.
  * @param label the label to be used on tabs or on download pages describing this choice
  * @param selected indicates whether this choice is one of the selected choices in the current render operation
  */
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
