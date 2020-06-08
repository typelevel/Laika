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

package laika.directive

import cats.data.{NonEmptySet, ValidatedNec}
import cats.implicits._
import laika.ast.Path.Root
import laika.config.{ArrayValue, BooleanValue, ConfigDecoder, ConfigError, ConfigValue, Key, NullValue, ObjectValue, StringValue}
import laika.ast.{SpanResolver, TemplateSpan, _}
import laika.bundle.BundleOrigin
import laika.rewrite.TemplateRewriter
import laika.rewrite.link.LinkConfig

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/** Provides the implementation for the standard directives included in Laika.
  *  
  * These include:
  * 
  * - `navigationTree`: Generates a navigation tree either automatically from specified root nodes
  *    of the input tree or by specifying entries manually (or a combination of both).
  * - `breadcrumb`: Builds a navigation list from the root node of the input tree to the current document.
  * - `ref`: Convenience directive that allows to reference a different section of the input tree
  *   via its headline text. The headline does not have to be unique, as long as the directive can identify
  *   a section that is closer than the others with the same headline.
  * - `api`: Convenience directive that allows to reference an api documentation entry (e.g. scaladoc, javadoc)
  * - `fragment`: Marks a block in a markup document as being separate from the main content, 
  *   so that it can be placed separately in templates.
  * - `for`: Accesses a value from the context and sets it as the reference context for its
  *   body elements, executing the body if the referenced value is non-empty and executing
  *   it multiple times when it is a collection.
  * - `if`: Accesses a value from the context and processes the body element only when
  *   it is a value recognized as true.
  * - `format`: Process the body element only when the output format matches the format
  *   specified in the directive (e.g. `pdf` or `html`).
  * - `style`: Adds a style property to the body element.
  * - `styleLink`: Adds link elements to HTML output for all CSS files found in the document tree
  * - `fragment`: Adds the body as a fragment to the target document, separate from the main
  *   content, to be rendered in different locations of the output, like headers, footers or sidebars.
  * - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based
  *   output, like XSL-FO or PDF.
  * 
  *  @author Jens Halm
  */
object StandardDirectives extends DirectiveRegistry {

  override val description: String = "Laika's built-in directives"

  override val origin: BundleOrigin = BundleOrigin.Library

  /** Implementation of the `for` directive for templates.
   */
  lazy val templateFor: Templates.Directive = Templates.create("for") {

    import Templates.dsl._

    val emptyValues = Set[ConfigValue](StringValue(""), BooleanValue(false), NullValue)
    case class Empty (spans: Seq[TemplateSpan])
    val emptySeparator = Templates.separator("empty", max = 1)(parsedBody.map(Empty))
    
    (attribute(0).as[String], separatedBody(Seq(emptySeparator)), cursor).mapN { (ref, multipart, cursor) =>
        
      def rewrite (spans: Seq[TemplateSpan], childCursor: DocumentCursor): TemplateSpanSequence =
        TemplateSpanSequence(spans).rewriteChildren(TemplateRewriter.rewriteRules(childCursor))
      
      def rewriteContent (value: ConfigValue): TemplateSpanSequence = rewrite(multipart.mainBody, cursor.withReferenceContext(value))

      def rewriteFallback = multipart.children.headOption.map(_.spans).map(rewrite(_, cursor)).getOrElse(TemplateSpanSequence.empty)
      
      cursor.resolveReference(Key.parse(ref)) match {
        case Right(Some(o: ObjectValue))             => rewriteContent(o) 
        case Right(Some(a: ArrayValue)) if a.isEmpty => rewriteFallback
        case Right(Some(a: ArrayValue))              => TemplateSpanSequence(a.values.map(rewriteContent))
        case Right(Some(simpleValue)) if emptyValues(simpleValue) => rewriteFallback
        case Right(Some(simpleValue))                => rewriteContent(simpleValue)
        case Right(None)                             => rewriteFallback
        case Left(error)                             => InvalidElement(s"Error retrieving reference '$ref': ${error.message}", "${"+ref+"}").asTemplateSpan
      }
    }
  }
  
  /** Implementation of the `if` directive for templates.
   */
  lazy val templateIf: Templates.Directive = Templates.create("if") {

    import Templates.dsl._
    
    val trueStrings = Set("true","yes","on","enabled")
    
    sealed trait IfSeparator extends Product with Serializable
    case class ElseIf (ref: String, body: Seq[TemplateSpan]) extends IfSeparator
    case class Else (body: Seq[TemplateSpan]) extends IfSeparator
    
    val elseIfSep = Templates.separator("elseIf") {
      (attribute(0).as[String], parsedBody).mapN(ElseIf) 
    }
    val elseSep = Templates.separator("else", max = 1) {
      parsedBody.map(Else)
    }
    val multipartBody = separatedBody(Seq(elseIfSep, elseSep))

    (attribute(0).as[String], multipartBody, cursor).mapN { (path, multipart, cursor) =>

      def rewrite (spans: Seq[TemplateSpan]): TemplateSpanSequence =
        TemplateSpanSequence(spans).rewriteChildren(TemplateRewriter.rewriteRules(cursor))
      
      def rewriteFallback = multipart.children
        .collectFirst { case e: Else => e }
        .map(_.body).map(rewrite)
        .getOrElse(TemplateSpanSequence.empty)
      
      @tailrec
      def process (parts: Seq[ElseIf]): TemplateSpanSequence = 
        if (parts.isEmpty) rewriteFallback
        else cursor.resolveReference(Key.parse(parts.head.ref)) match {
          case Right(Some(BooleanValue(true)))               => rewrite(parts.head.body)
          case Right(Some(StringValue(s))) if trueStrings(s) => rewrite(parts.head.body)
          case _ => process(parts.tail)
        }
      
      val alternatives = ElseIf(path, multipart.mainBody) +: multipart.collect[ElseIf] 
      process(alternatives)
    }
  }

  /** A block resolver that replaces itself with a navigation list from the root node of the input tree to the current document
    * during AST transformations.
    * 
    * Serves as the implementation for the breadcrumb directive, but can also be inserted into the AST manually.
    */
  case class BreadcrumbBuilder (options: Options = NoOpt) extends BlockResolver {

    type Self = BreadcrumbBuilder

    def resolve (cursor: DocumentCursor): Block = {
      
      val context = NavigationBuilderContext(
        refPath = cursor.path,
        itemStyles = Set(Style.breadcrumb.styles.head)
      )
      
      def entriesFor (tree: TreeCursor): Vector[NavigationItem] = {
        val title = tree.target.title.getOrElse(SpanSequence(tree.path.name))
        val item = context.newNavigationItem(title, tree.target.titleDocument.map(_.path), Nil)
        tree.parent.fold(Vector(item))(parent => entriesFor(parent) :+ item)
      }
      
      val docEntry = {
        val title = cursor.target.title.getOrElse(SpanSequence(cursor.path.name))
        context.newNavigationItem(title, Some(cursor.path), Nil)
      }
        
      NavigationList(entriesFor(cursor.parent) :+ docEntry, Style.breadcrumb)
    }

    def withOptions (options: Options): BreadcrumbBuilder = copy(options = options)

    lazy val unresolvedMessage: String = "Unresolved breadcrumb builder"
  }

  /** Implementation of the `breadcrumb` directive for templates.
    */
  lazy val templateBreadcrumb: Templates.Directive  = Templates.create("breadcrumb") {

    import Templates.dsl._

    cursor.map(c => TemplateElement(BreadcrumbBuilder().resolve(c)))
  }

  /** Implementation of the `breadcrumb` directive for block elements in markup documents.
    */
  lazy val blockBreadcrumb: Blocks.Directive  = Blocks.create("breadcrumb") {

    import Blocks.dsl._

    cursor.map(BreadcrumbBuilder().resolve)
  }

  /** A block resolver that replaces itself with a navigation list according to this instances configuration.
    * The resulting navigation tree can either be automatically generated from specified root nodes
    * of the input tree or by specifying entries manually (or a combination of both).
    *
    * Serves as the implementation for the navigationTree directive, but can also be inserted into the AST manually.
    * 
    * @param entries the list of manual and automatic entries to insert into the navigation tree
    * @param defaultDepth the depth for automatically generated entries (unless overridden in the entries' config)
    * @param itemStyles the styles to apply to all navigation items in the list as a render hint
    * @param excludeRoot indicates whether the root node should be excluded in automatic entries (may be overridden in the entries' config)
    * @param excludeSections indicates whether sections within documents should be excluded in automatic entries (may be overridden in the entries' config)
    * @param options optional styles and/or an id for the final navigation list
    */
  case class NavigationBuilderConfig (entries: Seq[NavigationNodeConfig], 
                                      defaultDepth: Int = Int.MaxValue, 
                                      itemStyles: Set[String] = Set(),
                                      excludeRoot: Boolean = false,
                                      excludeSections: Boolean = false,
                                      options: Options = NoOpt) extends BlockResolver {
    
    type Self = NavigationBuilderConfig

    /** Creates a navigation list for the specified document based on this instances configuration.
      * 
      * In case of configuration errors or references to non-existing documents an error message
      * will be returned as a `Left`.
      */
    def eval (cursor: DocumentCursor): Either[String, NavigationList] = {

      def generate (node: NavigationNodeConfig): ValidatedNec[String, List[NavigationItem]] = node match {

        case ManualNavigationNode(title, target, children) =>
          children.toList.map(generate).combineAll.map { childNodes =>
            target.fold[List[NavigationItem]](
              List(NavigationHeader(title, childNodes))
            ) { externalTarget =>
              List(NavigationLink(title, externalTarget, childNodes))
            }
          }

        case GeneratedNavigationNode(targetPath, title, depth, optExcludeRoot, optExcludeSections) =>
          val resolvedTarget = InternalTarget.fromPath(targetPath, cursor.path).absolutePath.relativeTo(Root)
          val target = cursor.root.target.tree.selectDocument(resolvedTarget).orElse(
            cursor.root.target.tree.selectSubtree(resolvedTarget)
          )
          target.fold[ValidatedNec[String, List[NavigationItem]]](
            s"Unable to resolve document or tree with path: $targetPath".invalidNec
          ) { treeContent =>
            val noRoot = optExcludeRoot.getOrElse(excludeRoot)
            val context = NavigationBuilderContext(
              refPath = cursor.path,
              itemStyles = itemStyles,
              maxLevels = depth.getOrElse(defaultDepth),
              currentLevel = if (noRoot) 0 else 1,
              excludeSections = optExcludeSections.getOrElse(excludeSections)
            )
            val navItem = treeContent.asNavigationItem(context)
            def replaceTitle (titleSpan: SpanSequence): NavigationItem = navItem match {
              case nh: NavigationHeader => nh.copy(title = titleSpan)
              case nl: NavigationLink   => nl.copy(title = titleSpan)
            }
            if (noRoot) navItem.content.toList.validNec
            else List(title.fold(navItem)(replaceTitle)).validNec
          }
      }

      entries.toList
        .map(generate)
        .combineAll
        .toEither
        .map(NavigationList(_))
        .leftMap(errors => s"One or more errors generating navigation: ${errors.toList.mkString(",")}")
    }

    def resolve (cursor: DocumentCursor): Block =
      eval(cursor).fold(error => InvalidElement(error, error).asBlock, identity)

    def withOptions (options: Options): NavigationBuilderConfig = copy(options = options)

    lazy val unresolvedMessage: String = "Unresolved navigation builder"
  }

  /** Companion with a decoder for obtaining instances from HOCON.
    */
  object NavigationBuilderConfig {
    
    implicit val decoder: ConfigDecoder[NavigationBuilderConfig] = ConfigDecoder.config.flatMap { config =>
      for {
        entries         <- config.get[Seq[NavigationNodeConfig]]("entries", Nil)
        defaultDepth    <- config.get[Int]("defaultDepth", Int.MaxValue)
        itemStyles      <- config.get[Seq[String]]("itemStyles", Nil)
        excludeRoot     <- config.get[Boolean]("excludeRoot", false)
        excludeSections <- config.get[Boolean]("excludeSections", false)
      } yield NavigationBuilderConfig(entries, defaultDepth, itemStyles.toSet, excludeRoot, excludeSections)
    }
    
  }

  /** Represents the configuration for a single node in a navigation tree.
    */
  sealed trait NavigationNodeConfig

  /** Companion with a decoder for obtaining instances from HOCON.
    */
  object NavigationNodeConfig {

    implicit lazy val decoder: ConfigDecoder[NavigationNodeConfig] = ConfigDecoder.config.flatMap { config =>
      
      config.getOpt[String]("target").flatMap { optTarget =>
        
        def createManualNode (externalTarget: Option[ExternalTarget]): Either[ConfigError, NavigationNodeConfig] = for {
            title    <- config.get[String]("title")
            children <- config.get[Seq[NavigationNodeConfig]]("children", Nil)(ConfigDecoder.seq(decoder))
          } yield {
            ManualNavigationNode(SpanSequence(title), externalTarget, children)
          }

        def createGeneratedNode (internalTarget: PathBase): Either[ConfigError, NavigationNodeConfig] = for {
            title           <- config.getOpt[String]("title")
            depth           <- config.getOpt[Int]("depth")
            excludeRoot     <- config.getOpt[Boolean]("excludeRoot")
            excludeSections <- config.getOpt[Boolean]("excludeSections")
          } yield {
            val titleSpan = title.map(SpanSequence(_))
            GeneratedNavigationNode(internalTarget, titleSpan, depth, excludeRoot, excludeSections)
          }

        optTarget.fold(createManualNode(None)) { targetStr =>
          if (targetStr.startsWith("http:") || targetStr.startsWith("https:")) 
            createManualNode(Some(ExternalTarget(targetStr)))
          else
            createGeneratedNode(PathBase.parse(targetStr))
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
  case class GeneratedNavigationNode (target: PathBase,
                                      title: Option[SpanSequence] = None, 
                                      depth: Option[Int] = None,
                                      excludeRoot: Option[Boolean] = None,
                                      excludeSections: Option[Boolean] = None) extends NavigationNodeConfig

  /** The configuration for a manual entry in the navigation tree.
    * The entry can have further children which may in turn be either manual or automatically generated nodes.
    * 
    * @param title    the title for this entry when getting rendered as a link
    * @param target   the external link for this node (if missing this node just generates a navigation header as a separator within the tree)
    * @param children the children of this node, either manual or automatically generated
    */
  case class ManualNavigationNode (title: SpanSequence, 
                                   target: Option[ExternalTarget] = None, 
                                   children: Seq[NavigationNodeConfig] = Nil) extends NavigationNodeConfig

  /** Implementation of the `navigationTree` directive for templates.
    */
  lazy val templateNav: Templates.Directive  = Templates.eval("navigationTree") {

    import Templates.dsl._

    (allAttributes, cursor).mapN { (config, cursor) =>
      config.get[NavigationBuilderConfig](Key.root)
        .leftMap(_.message)
        .flatMap(_.eval(cursor).map(TemplateElement(_)))
    }
  }

  /** Implementation of the `navigationTree` directive for block elements in markup documents.
    */
  lazy val blockNav: Blocks.Directive  = Blocks.eval("navigationTree") {

    import Blocks.dsl._

    (allAttributes, cursor).mapN { (config, cursor) =>
      config.get[NavigationBuilderConfig](Key.root)
        .leftMap(_.message)
        .flatMap(_.eval(cursor))
    }
  }

  /** Implementation of the `api` directive that creates links to API documentation based
    * on a specified fully-qualified type name. The type name is the only (required) attribute
    * of the directive.
    *
    * The directive relies on base URIs defined in the transformation's configuration and will
    * otherwise fail. See [[laika.rewrite.link.LinkConfig]] for details.
    */
  lazy val api: Links.Directive = Links.eval("api") { (linkId, cursor) =>
    cursor.config
      .getOpt[LinkConfig]
      .map(_.map(_.apiLinks).getOrElse(Nil))
      .leftMap(_.message)
      .flatMap { apiLinks =>
        val matching = apiLinks.toList.filter(l => linkId.startsWith(l.packagePrefix)).maximumByOption(_.packagePrefix.length)
        matching.orElse(apiLinks.find(_.packagePrefix == "*")).fold[Either[String, SpanLink]] (
          Left(s"No base URI defined for '$linkId' and no default URI available.")
        ) { link => 
          def splitAtLast(in: String, char: Char) = in.split(char).toSeq match {
            case Seq(single)  => (single, None)
            case init :+ last => (init.mkString(char.toString), Some(last))
          }
          val (fqName, method) = splitAtLast(linkId, '#')
          val (packageName, className) = splitAtLast(fqName, '.')
          val isPackage = className.contains("package")
          val typeText = if (isPackage) packageName else className.getOrElse(fqName)
          val text = typeText + method.fold("")(m => "." + m.split('(').head)
          val typePath = 
            if (isPackage) packageName.replaceAllLiterally(".", "/") + "/" + link.packageSummary 
            else fqName.replaceAllLiterally(".", "/") + ".html"
          val uri = link.baseUri + typePath + method.fold("")("#" + _)
          Right(SpanLink(Seq(Text(text)), Target.create(uri)))
        }
      }
  } 
  
  
  private def asBlock (blocks: Seq[Block], options: Options = NoOpt): Block = blocks match {
    case block :: Nil => block.mergeOptions(options)
    case multiple     => BlockSequence(multiple, options)
  }
  
  private def asSpan (spans: Seq[Span], options: Options = NoOpt): Span = spans match {
    case span :: Nil => span.mergeOptions(options)
    case multiple    => SpanSequence(multiple, options)
  }
  
  
  /** Implementation of the `for` directive for block elements in markup documents.
   *  The content of such a block will only be rendered for the corresponding
   *  output format (e.g. `pdf` or `html`).
   */
  lazy val format: Blocks.Directive  = Blocks.eval("format") {
    import Blocks.dsl._
    
    (positionalAttributes.as[String].widen, parsedBody.map(asBlock(_))).mapN { (formats, body) =>
      NonEmptySet
        .fromSet(TreeSet(formats:_*))
        .fold[Either[String, Block]](Left("no formats provided"))(set => Right(TargetFormat(set, body)))
    }
  }
  
  /** Implementation of the `style` directive for block elements in markup documents.
   */
  lazy val blockStyle: Blocks.Directive  = Blocks.create("style") {
    import Blocks.dsl._
    
    (parsedBody, attribute(0).as[String].map(Styles(_))).mapN(asBlock)
  }
  
  /** Implementation of the `style` directive for span elements in markup documents.
   */
  lazy val spanStyle: Spans.Directive  = Spans.create("style") {
    import Spans.dsl._

    (parsedBody, attribute(0).as[String].map(Styles(_))).mapN(asSpan)
  }
  
  /** Implementation of the `fragment` directive for block elements in markup documents.
   */
  lazy val blockFragment: Blocks.Directive  = Blocks.create("fragment") {
    import Blocks.dsl._
    
    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      DocumentFragment(name, asBlock(content, Styles(name)))
    }
  }
  
  /** Implementation of the `fragment` directive for templates.
   */
  lazy val templateFragment: Templates.Directive  = Templates.create("fragment") {
    import Templates.dsl._
    
    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }
  
  /** Implementation of the `pageBreak` directive.
   */
  lazy val pageBreak: Blocks.Directive = Blocks.create("pageBreak") {
    import Blocks.dsl._
    
    empty(PageBreak())
  }
  
  /** The complete list of standard directives for block
   *  elements in markup documents.
   */
  lazy val blockDirectives: Seq[Blocks.Directive] = List(
    blockBreadcrumb,
    blockNav,
    blockFragment,
    blockStyle,
    format,
    pageBreak
  )
  
  /** The complete list of standard directives for span
   *  elements in markup documents.
   */
  lazy val spanDirectives: Seq[Spans.Directive] = List(
    spanStyle
  )

  /** Template resolver that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    *
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    * 
    * This is an alternative to the `@styleLinks` directive that can be used where
    * template ASTs are created programmatically for extensions.
    */
  case object StyleLinks extends SpanResolver with TemplateSpan {
    type Self = this.type
    def withOptions (options: Options): this.type = this
    val options: Options = NoOpt

    def resolve (cursor: DocumentCursor): TemplateElement = {
      val allLinks = cursor.root.target.staticDocuments.filter(_.suffix.contains("css")).map { staticPath =>
        val path = staticPath.relativeTo(cursor.path).toString
        s"""<link rel="stylesheet" type="text/css" href="$path" />"""
      }
      TemplateElement(RawContent(Seq("html","xhtml"), allLinks.mkString("\n    ")))
    }
    lazy val unresolvedMessage: String = s"Unresolved style links generator"
  }

  /** Template directive that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    * 
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val styleLinksDirective: Templates.Directive = Templates.create("styleLinks") {
    Templates.dsl.cursor.map(StyleLinks.resolve)
  }

  /** The complete list of standard directives for templates.
   */
  lazy val templateDirectives: Seq[Templates.Directive] = List(
    templateBreadcrumb,
    templateNav,
    templateFor,
    templateIf,
    styleLinksDirective
  )

  /** The complete list of standard directives for links.
    */
  lazy val linkDirectives: Seq[Links.Directive] = Seq(api)
  
}
