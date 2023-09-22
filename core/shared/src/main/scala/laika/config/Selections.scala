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

package laika.config

import cats.data.{ Chain, NonEmptyChain }
import laika.api.config.Config.ConfigResult
import laika.api.config.{ Config, ConfigBuilder, ConfigDecoder, ConfigEncoder, DefaultKey }
import laika.ast.*
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.config.Selections.Classifiers

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
sealed abstract class Selections {

  /** All selection configurations of this instance.
    */
  def all: Seq[SelectionConfig]

  /** Returns the selection with the specified name, if present.
    */
  def getSelection(name: String): Option[SelectionConfig] = all.find(_.name == name)

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
  def getClassifiers: Classifiers = Classifiers(all.flatMap { group =>
    group.choices.find(_.selected).map(_.name)
  })

}

/** Companion for creating selection config instances that can be passed to Laika configuration
  * and builders that produce configuration of all possible combinations of @:select directives.
  *
  * See the documentation for the `@:select` directive in the manual for the full context of this feature.
  */
object Selections {

  case class Classifiers(value: Seq[String])

  private final case class Impl(all: Seq[SelectionConfig]) extends Selections {
    override def productPrefix: String = "Selections"
  }

  implicit val key: DefaultKey[Selections] = DefaultKey(LaikaKeys.selections)

  implicit val decoder: ConfigDecoder[Selections] =
    ConfigDecoder.seq[SelectionConfig].map(Impl.apply)

  implicit val encoder: ConfigEncoder[Selections] =
    ConfigEncoder.seq[SelectionConfig].contramap(_.all)

  val empty: Selections = Impl(Nil)

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
  def apply(selections: SelectionConfig*): Selections = Impl(selections)

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
  def createCombinationsConfig(config: Config): ConfigResult[Seq[Seq[ChoiceConfig]]] = {

    def createCombinations(value: Option[Selections]): Seq[Seq[ChoiceConfig]] =
      value.getOrElse(Selections.empty).all
        .filter(_.separateEbooks)
        .map(_.choices.toChain.toList.map(List(_)))
        .reduceLeftOption { (as, bs) =>
          for { a <- as; b <- bs } yield a ++ b
        }
        .getOrElse(Nil)

    config.getOpt[Selections].map(createCombinations)
  }

  private[laika] def createCombinations(
      config: Config
  ): ConfigResult[NonEmptyChain[(Config, Classifiers)]] = {

    val baseKey = "metadata.identifier"

    for {
      selections      <- config.getOpt[Selections].map(_.getOrElse(Selections.empty))
      epubCoverImages <- CoverImages.forEPUB(config)
      pdfCoverImages  <- CoverImages.forPDF(config)
      defaultId       <- config.getOpt[String](baseKey)
      epubId          <- config.getOpt[String](s"epub.$baseKey").map(_.orElse(defaultId))
      pdfId           <- config.getOpt[String](s"pdf.$baseKey").map(_.orElse(defaultId))
    } yield {

      def createCombinations(value: Selections): NonEmptyChain[Selections] = {
        val (separated, nonSeparated) = value.all.partition(_.separateEbooks)

        val combinations = separated
          .map { group =>
            group.choices.map(choice => NonEmptyChain.one(group.select(choice)))
          }
          .reduceLeftOption { (as, bs) =>
            for { a <- as; b <- bs } yield a ++ b
          }

        combinations.fold(NonEmptyChain.one(value))(_.map { combined =>
          Selections((combined.toChain.toList ++ nonSeparated) *)
        })
      }

      def populateConfig(value: Selections, configBuilder: ConfigBuilder): ConfigBuilder = {
        val classifier = value.getClassifiers.value.mkString("-")
        val withEPUB   = epubCoverImages.getImageFor(classifier).fold(configBuilder) { img =>
          configBuilder.withValue(
            LaikaKeys.root.child("epub").child(LaikaKeys.coverImage.local),
            img
          )
        }
        val withImages = pdfCoverImages.getImageFor(classifier).fold(withEPUB) { img =>
          withEPUB.withValue(LaikaKeys.root.child("pdf").child(LaikaKeys.coverImage.local), img)
        }
        if (classifier.isEmpty) withImages
        else
          withImages
            .withValue(s"epub.$baseKey", epubId.map(_ + classifier))
            .withValue(s"pdf.$baseKey", pdfId.map(_ + classifier))
      }

      createCombinations(selections).map { newConfig =>
        val populatedConfig = populateConfig(newConfig, config.withValue(newConfig)).build
        (populatedConfig, newConfig.getClassifiers)
      }
    }
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
  def createCombinations(
      root: DocumentTreeRoot
  ): ConfigResult[NonEmptyChain[(DocumentTreeRoot, Classifiers)]] = {
    createCombinations(root.config).map(_.map { case (config, classifiers) =>
      (root.withConfig(config), classifiers)
    })
  }

  private[laika] object FormatFilter extends RewriteRulesBuilder {

    def apply(cursor: DocumentCursor): ConfigResult[RewriteRules] = {

      // maps selection name to selected choice name
      def extractMap(selectionConfig: Selections): Map[String, String] = selectionConfig.all
        .flatMap(selection => selection.choices.find(_.selected).map(c => (selection.name, c.name)))
        .toMap

      val targetConfig =
        if (cursor.root.config.hasKey(LaikaKeys.selections)) cursor.root.config else cursor.config

      val selections: ConfigResult[Map[String, String]] = targetConfig
        .getOpt[Selections]
        .map(_.getOrElse(Selections.empty))
        .map(extractMap)

      def select(selection: Selection, selectedChoice: String): Block = selection.choices
        .find(_.name == selectedChoice)
        .fold[Block](selection)(choice => BlockSequence(choice.content))

      selections.map { selections =>
        if (selections.isEmpty) RewriteRules.empty
        else
          RewriteRules.forBlocks {
            case sel: Selection if selections.contains(sel.name) =>
              RewriteAction.Replace(select(sel, selections(sel.name)))
          }
      }
    }

  }

}

/** Configuration for a single kind of selection and its choices available for the user.
  */
sealed abstract class SelectionConfig {

  /** The name of the selection as used in text markup, e.g. `@:select(name)`. */
  def name: String

  /** The configuration for one or more choices that are available in each of the directives. */
  def choices: NonEmptyChain[ChoiceConfig]

  /** Indicates whether the selection should render all its choices in the same output
    * or produce separate e-books where in each of them only one of the choice is displayed.
    * This way separate e-books for Scala vs. Java code samples or sbt vs Maven build
    * examples can be produced.
    *
    * Keep in mind that multiple selections having this property set to true would result
    * in the cartesian product of available e-book versions,
    * it is therefore unusual to have more than one or two.
    */
  def separateEbooks: Boolean

  /** Specifies that the choices of this selection should be rendered in entirely separate e-books
    * and not below each other in the same output.
    */
  def withSeparateEbooks: SelectionConfig

  /** Returns the label of the choice with the specified name, if present.
    */
  def getLabel(name: String): Option[String] = choices.find(_.name == name).map(_.label)

  /** Returns a copy of this instance where the specified choice is marked as selected.
    */
  def select(choice: ChoiceConfig): SelectionConfig

}

object SelectionConfig {

  private final case class Impl(
      name: String,
      choices: NonEmptyChain[ChoiceConfig],
      separateEbooks: Boolean = false
  ) extends SelectionConfig {

    override def productPrefix: String = "SelectionConfig"

    def withSeparateEbooks: SelectionConfig = copy(separateEbooks = true)

    def select(choice: ChoiceConfig): SelectionConfig =
      copy(choices = choices.map(c => if (c == choice) c.select else c))

  }

  /** Creates a new configuration instance for a single kind of selection and its choices available for the user.
    */
  def apply(name: String, choice: ChoiceConfig, choices: ChoiceConfig*): SelectionConfig =
    Impl(name, NonEmptyChain.fromChainPrepend(choice, Chain.fromSeq(choices)))

  implicit val decoder: ConfigDecoder[SelectionConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name     <- config.get[String]("name")
      choices  <- config.get[NonEmptyChain[ChoiceConfig]]("choices")
      separate <- config.get[Boolean]("separateEbooks", false)
    } yield {
      Impl(name, choices, separate)
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
  */
sealed abstract class ChoiceConfig {

  /** The name of the selection as used in text markup, e.g. `@:choice(name)`. */
  def name: String

  /** The label to be used on tabs or on download pages describing this choice. */
  def label: String

  /** Indicates whether this choice is one of the selected choices in the current render operation. */
  def selected: Boolean

  /** Creates a copy with the selected flag set to true. */
  def select: ChoiceConfig
}

object ChoiceConfig {

  def apply(name: String, label: String): ChoiceConfig = Impl(name, label, selected = false)

  private final case class Impl(name: String, label: String, selected: Boolean)
      extends ChoiceConfig {
    override def productPrefix: String = "ChoiceConfig"
    def select: ChoiceConfig           = copy(selected = true)
  }

  implicit val decoder: ConfigDecoder[ChoiceConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      name     <- config.get[String]("name")
      label    <- config.get[String]("label")
      selected <- config.get[Boolean]("selected", false)
    } yield {
      Impl(name, label, selected)
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
