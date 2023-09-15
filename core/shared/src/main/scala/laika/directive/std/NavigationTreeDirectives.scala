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

package laika.directive.std

import cats.syntax.all._
import cats.data.ValidatedNec
import laika.api.bundle.{ BlockDirectives, TemplateDirectives }
import laika.api.config.{ ConfigDecoder, ConfigError, Key }
import laika.ast.{
  Block,
  BlockResolver,
  DocumentCursor,
  ExternalTarget,
  InternalTarget,
  InvalidBlock,
  NavigationBuilderContext,
  NavigationItem,
  NavigationLink,
  NavigationList,
  NoOpt,
  Options,
  RewritePhase,
  SpanSequence,
  Style,
  TemplateElement,
  VirtualPath
}
import laika.parse.{ GeneratedSource, SourceFragment }

/** Implementation of the navigationTree directive for templates and markup blocks.
  *
  * This directive supports the generation of navigation trees which can be any combination of auto-generated
  * trees from the input tree and manual entries, optionally to external targets, too.
  *
  * For full documentation see the section about the
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#navigationtree navigationTree Directive]]
  * in the manual.
  *
  * @author Jens Halm
  */
private[laika] object NavigationTreeDirectives {

  /** A block resolver that replaces itself with a navigation list according to this instances configuration.
    * The resulting navigation tree can either be automatically generated from specified root nodes
    * of the input tree or by specifying entries manually (or a combination of both).
    *
    * Serves as the implementation for the navigationTree directive, but can also be inserted into the AST manually.
    *
    * @param entries the list of manual and automatic entries to insert into the navigation tree
    * @param source the source fragment that produced this navigation builder, for error reporting
    * @param defaultDepth the depth for automatically generated entries (unless overridden in the entries' config)
    * @param itemStyles the styles to apply to all navigation items in the list as a render hint
    * @param excludeRoot indicates whether the root node should be excluded in automatic entries (may be overridden in the entries' config)
    * @param excludeSections indicates whether sections within documents should be excluded in automatic entries (may be overridden in the entries' config)
    * @param excludeSelf indicates whether the current document should be included
    * @param options optional styles and/or an id for the final navigation list
    */
  case class NavigationBuilderConfig(
      entries: Seq[NavigationNodeConfig],
      source: SourceFragment,
      defaultDepth: Int = Int.MaxValue,
      itemStyles: Set[String] = Set(),
      excludeRoot: Boolean = false,
      excludeSections: Boolean = false,
      excludeSelf: Boolean = false,
      options: Options = NoOpt
  ) extends BlockResolver {

    type Self = NavigationBuilderConfig

    /** Creates a navigation list for the specified document based on this instances configuration.
      *
      * In case of configuration errors or references to non-existing documents an error message
      * will be returned as a `Left`.
      */
    def eval(cursor: DocumentCursor): Either[String, NavigationList] = {

      def generate(
          currentLevel: Int
      )(node: NavigationNodeConfig): ValidatedNec[String, List[NavigationItem]] = node match {

        case ManualNavigationNode(title, target, entries) =>
          val link = target.map(NavigationLink(_))
          if (currentLevel < defaultDepth)
            entries.toList.map(generate(currentLevel + 1)).combineAll.map { childNodes =>
              List(NavigationItem(title, childNodes, link, options = Style.level(currentLevel)))
            }
          else if (link.nonEmpty) List(NavigationItem(title, Nil, link)).validNec
          else Nil.validNec

        case GeneratedNavigationNode(
              targetPath,
              title,
              depth,
              optExcludeRoot,
              optExcludeSections
            ) =>
          val resolvedTarget =
            InternalTarget(targetPath).relativeTo(cursor.path).absolutePath.relative
          val target         = cursor.root.target.tree.selectDocument(resolvedTarget).orElse(
            cursor.root.target.tree.selectSubtree(resolvedTarget)
          )
          target.fold[ValidatedNec[String, List[NavigationItem]]](
            s"Unable to resolve document or tree with path: $targetPath".invalidNec
          ) { treeContent =>
            val noRoot  = optExcludeRoot.getOrElse(excludeRoot)
            val context = NavigationBuilderContext.defaults
              .withRefPath(cursor.path)
              .withItemStyles(itemStyles.toSeq *)
              .withMaxLevels(depth.getOrElse(defaultDepth))
              .withExcludeSections(optExcludeSections.getOrElse(excludeSections))
              .withExcludeSelf(excludeSelf)
              .withCurrentLevel(if (noRoot) currentLevel - 1 else currentLevel)

            val navItem = treeContent.asNavigationItem(context)
            if (noRoot) navItem.content.toList.validNec
            else List(title.fold(navItem)(t => navItem.copy(title = t))).validNec
          }
      }

      entries.toList
        .map(generate(1))
        .combineAll
        .toEither
        .map(NavigationList(_))
        .leftMap(errors =>
          s"One or more errors generating navigation: ${errors.toList.mkString(",")}"
        )
    }

    def resolve(cursor: DocumentCursor): Block =
      eval(cursor).fold(InvalidBlock(_, source), identity)

    def withOptions(options: Options): NavigationBuilderConfig = copy(options = options)

    def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

    lazy val unresolvedMessage: String = "Unresolved navigation builder"
  }

  /** Companion with a decoder for obtaining instances from HOCON.
    */
  object NavigationBuilderConfig {

    implicit val decoder: ConfigDecoder[NavigationBuilderConfig] = ConfigDecoder.config.flatMap {
      config =>
        for {
          entries         <- config.get[Seq[NavigationNodeConfig]]("entries", Nil)
          defaultDepth    <- config.get[Int]("defaultDepth", Int.MaxValue)
          itemStyles      <- config.get[Seq[String]]("itemStyles", Nil)
          excludeRoot     <- config.get[Boolean]("excludeRoot", false)
          excludeSections <- config.get[Boolean]("excludeSections", false)
          excludeSelf     <- config.get[Boolean]("excludeSelf", false)
        } yield {
          NavigationBuilderConfig(
            entries,
            GeneratedSource,
            defaultDepth,
            itemStyles.toSet,
            excludeRoot,
            excludeSections,
            excludeSelf
          )
        }
    }

  }

  /** Represents the configuration for a single node in a navigation tree.
    */
  sealed trait NavigationNodeConfig

  /** Companion with a decoder for obtaining instances from HOCON.
    */
  object NavigationNodeConfig {

    implicit lazy val decoder: ConfigDecoder[NavigationNodeConfig] = ConfigDecoder.config.flatMap {
      config =>
        config.getOpt[String]("target").flatMap { optTarget =>
          def createManualNode(
              externalTarget: Option[ExternalTarget]
          ): Either[ConfigError, NavigationNodeConfig] = for {
            title    <- config.get[String]("title")
            children <- config.get[Seq[NavigationNodeConfig]]("entries", Nil)(
              ConfigDecoder.seq(decoder)
            )
          } yield {
            ManualNavigationNode(SpanSequence(title), externalTarget, children)
          }

          def createGeneratedNode(
              internalTarget: VirtualPath
          ): Either[ConfigError, NavigationNodeConfig] = for {
            title           <- config.getOpt[String]("title")
            depth           <- config.getOpt[Int]("depth")
            excludeRoot     <- config.getOpt[Boolean]("excludeRoot")
            excludeSections <- config.getOpt[Boolean]("excludeSections")
          } yield {
            val titleSpan = title.map(SpanSequence(_))
            GeneratedNavigationNode(internalTarget, titleSpan, depth, excludeRoot, excludeSections)
          }

          optTarget.fold(createManualNode(None)) { targetStr =>
            if (
              targetStr.startsWith("http:") || targetStr.startsWith(
                "https:"
              ) || targetStr.startsWith("mailto:")
            )
              createManualNode(Some(ExternalTarget(targetStr)))
            else
              createGeneratedNode(VirtualPath.parse(targetStr))
          }
        }
    }

  }

  /** The configuration for an automatically generated navigation tree.
    *
    * @param target      the target to use as the root node for navigation tree based on a relative or absolute path in the virtual input tree
    * @param title       the title for this entry when getting rendered as a link
    * @param depth       the depth to recurse from the root node
    * @param excludeRoot indicates whether the root node should be excluded in which case the first-level children will be inserted into the parent node
    * @param excludeSections indicates whether sections within documents should be excluded in automatic entries
    */
  case class GeneratedNavigationNode(
      target: VirtualPath,
      title: Option[SpanSequence] = None,
      depth: Option[Int] = None,
      excludeRoot: Option[Boolean] = None,
      excludeSections: Option[Boolean] = None
  ) extends NavigationNodeConfig

  /** The configuration for a manual entry in the navigation tree.
    * The entry can have further children which may in turn be either manual or automatically generated nodes.
    *
    * @param title    the title for this entry when getting rendered as a link
    * @param target   the external link for this node (if missing this node just generates a navigation header as a separator within the tree)
    * @param entries  the children of this node, either manual or automatically generated
    */
  case class ManualNavigationNode(
      title: SpanSequence,
      target: Option[ExternalTarget] = None,
      entries: Seq[NavigationNodeConfig] = Nil
  ) extends NavigationNodeConfig

  /** Implementation of the `navigationTree` directive for templates.
    */
  lazy val forTemplates: TemplateDirectives.Directive = TemplateDirectives.eval("navigationTree") {

    import TemplateDirectives.dsl._

    (allAttributes, cursor).mapN { (config, cursor) =>
      config.get[NavigationBuilderConfig](Key.root)
        .leftMap(_.message)
        .flatMap(_.eval(cursor).map(TemplateElement(_)))
    }
  }

  /** Implementation of the `navigationTree` directive for block elements in markup documents.
    */
  lazy val forBlocks: BlockDirectives.Directive = BlockDirectives.eval("navigationTree") {

    import BlockDirectives.dsl._

    (allAttributes, cursor).mapN { (config, cursor) =>
      config.get[NavigationBuilderConfig](Key.root)
        .leftMap(_.message)
        .flatMap(_.eval(cursor))
    }
  }

}
