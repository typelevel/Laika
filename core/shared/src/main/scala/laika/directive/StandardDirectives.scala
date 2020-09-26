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

import cats.data.{NonEmptyChain, NonEmptySet, ValidatedNec}
import cats.implicits._
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.BundleOrigin
import laika.config._
import laika.parse.{LineSource, SourceCursor, SourceFragment}
import laika.rewrite.TemplateRewriter
import laika.rewrite.link.LinkConfig
import laika.rewrite.nav.Selections

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
  * - `source`: Convenience directive that allows to reference a hosted source (e.g. on GitHub)
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
  * - `linkCSS`: Adds link elements to HTML/EPUB output for all or selected CSS files found in the document tree
  * - `linkJS`: Adds link elements to HTML/EPUB output for all or selected JavaScript files found in the document tree
  * - `fragment`: Adds the body as a fragment to the target document, separate from the main
  *   content, to be rendered in different locations of the output, like headers, footers or sidebars.
  * - `relativePath`: Translates an absolute or relative path from the perspective of a template
  *   to a path relative to the document the template had been applied to
  * - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based
  *   output, like XSL-FO or PDF.
  * - `todo`: simple directive that accepts a string argument that will be ignored by renderers,
  *   overcoming the limitation that Markdown does not have a native comment syntax.
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
    
    (attribute(0).as[String], separatedBody(Seq(emptySeparator)), cursor, Templates.dsl.source).mapN { (ref, multipart, cursor, source) =>
        
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
        case Left(error)                             => InvalidElement(s"Error retrieving reference '$ref': ${error.message}", source).asTemplateSpan
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
          case Right(Some(a: ArrayValue)) if !a.isEmpty      => rewrite(parts.head.body)
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
  case class BreadcrumbBuilder (source: SourceFragment, options: Options = NoOpt) extends BlockResolver {

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

    (cursor, Templates.dsl.source).mapN { case (cursor, src) =>
      TemplateElement(BreadcrumbBuilder(src).resolve(cursor))
    }
  }

  /** Implementation of the `breadcrumb` directive for block elements in markup documents.
    */
  lazy val blockBreadcrumb: Blocks.Directive  = Blocks.create("breadcrumb") {

    import Blocks.dsl._

    (cursor, Blocks.dsl.source).mapN { case (cursor, src) =>
      BreadcrumbBuilder(src).resolve(cursor)
    }
  }

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
  case class NavigationBuilderConfig (entries: Seq[NavigationNodeConfig],
                                      source: SourceFragment,
                                      defaultDepth: Int = Int.MaxValue, 
                                      itemStyles: Set[String] = Set(),
                                      excludeRoot: Boolean = false,
                                      excludeSections: Boolean = false,
                                      excludeSelf: Boolean = false,
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
          val resolvedTarget = InternalTarget(targetPath).relativeTo(cursor.path).absolutePath.relative
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
              excludeSections = optExcludeSections.getOrElse(excludeSections),
              excludeSelf = excludeSelf
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

    def resolve (cursor: DocumentCursor): Block = eval(cursor).fold(InvalidBlock(_, source), identity)

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
        excludeSelf     <- config.get[Boolean]("excludeSelf", false)
      } yield {
        NavigationBuilderConfig(entries, LineSource("", SourceCursor("")), defaultDepth, itemStyles.toSet, excludeRoot, excludeSections, excludeSelf)
      }
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
  
  private def linkConfig[T] (cursor: DocumentCursor, extract: LinkConfig => Seq[T]): Either[String, Seq[T]] =
    linkConfig(cursor).map(extract)

  private def linkConfig[T] (cursor: DocumentCursor): Either[String, LinkConfig] =
    cursor.config
      .getOpt[LinkConfig]
      .map(_.getOrElse(LinkConfig()))
      .leftMap(_.message)

  /** Implementation of the `api` directive that creates links to API documentation based
    * on a specified fully-qualified type name. The type name is the only (required) attribute
    * of the directive.
    *
    * The directive relies on base URIs defined in the transformation's configuration and will
    * otherwise fail. See [[laika.rewrite.link.LinkConfig]] for details.
    */
  lazy val api: Links.Directive = Links.eval("api") { (linkId, cursor) =>
    linkConfig(cursor)
      .flatMap { linkConfig =>
        val matching = linkConfig.apiLinks.toList.filter(l => linkId.startsWith(l.packagePrefix)).maximumByOption(_.packagePrefix.length)
        matching.orElse(linkConfig.apiLinks.find(_.packagePrefix == "*")).fold[Either[String, SpanLink]] (
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
            if (isPackage) packageName.replace(".", "/") + "/" + link.packageSummary 
            else fqName.replace(".", "/") + ".html"
          val uri = link.baseUri + typePath + method.fold("")("#" + _)
          val target = Target.parse(uri) match {
            case et: ExternalTarget => et
            case it: InternalTarget =>
              val resolved = it.relativeTo(cursor.path)
              val externalUrl = linkConfig.internalLinkMappings.collectFirst {
                case mapping if resolved.absolutePath.isSubPath(mapping.internalPath) => 
                  mapping.externalBaseUrl + resolved.absolutePath.relativeTo(mapping.internalPath / "ref").toString // TODO - move this logic into model, it's used in multiple places
              }
              resolved.copy(externalUrl = externalUrl)
          }
          Right(SpanLink(Seq(Text(text)), target))
        }
      }
  }

  /** Implementation of the `source` directive that creates links to hosted sources based
    * on a specified fully-qualified type name or a path to markup source file. 
    * The type name or path is the only (required) attribute of the directive.
    *
    * The directive relies on base URIs defined in the transformation's configuration and will
    * otherwise fail. See [[laika.rewrite.link.LinkConfig]] for details.
    */
  lazy val source: Links.Directive = Links.eval("source") { (linkId, cursor) =>
    linkConfig(cursor, _.sourceLinks)
      .flatMap { sourceLinks =>
        val matching = sourceLinks.toList.filter(l => linkId.startsWith(l.packagePrefix)).maximumByOption(_.packagePrefix.length)
        matching.orElse(sourceLinks.find(_.packagePrefix == "*")).fold[Either[String, SpanLink]] (
          Left(s"No base URI defined for '$linkId' and no default URI available.")
        ) { link =>
          val typePath = linkId.replace(".", "/") + "." + link.suffix
          val uri = link.baseUri + typePath
          val text = linkId.split('.').last
          Right(SpanLink(Seq(Text(text)), Target.parse(uri)))
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

  /** Implementation of the `callout` directive for block elements in markup documents.
    * The body of such a directive will get two styles assigned: `callout` and the argument
    * passed to the directive (e.g. `@:callout(info)`).
    */
  lazy val callout: Blocks.Directive = Blocks.create("callout") {
    import Blocks.dsl._

    (attribute(0).as[String].widen, parsedBody).mapN { (style, body) =>
      BlockSequence(body, Styles("callout", style))
    }
  }

  /** Implementation of the `select` directive for block elements in markup documents.
    * 
    * Selections represent the same content in different ways,
    * e.g. a code sample in Scala or Java or a build setup in sbt vs. Maven.
    * In the final output these will usually be rendered in a way to allow for a convenient selection by the user.
    * 
    * A valid `@:select` directive has at least two separator directives (`@:choice`) in the body.
    */
  val selectDirective: Blocks.Directive = Blocks.eval("select") {
    import Blocks.dsl._

    val separator: Blocks.SeparatorDirective[Choice] = Blocks.separator("choice", min = 2) {
      (attribute(0).as[String], parsedBody).mapN { (name, body) =>
        Choice(name, name, body)
      }
    }
    
    (attribute(0).as[String], separatedBody(Seq(separator)), cursor).mapN { (name, multiPart, cursor) =>
      cursor.config.get[Selections]
        .leftMap(e => s"Error reading config for selections: ${e.message}")
        .flatMap { config =>
          config.getSelection(name).toRight(s"Not found: selection '$name'").flatMap { selection =>
            multiPart
              .children.toList
              .map { choice =>
                selection
                  .getLabel(choice.name)
                  .map(label => choice.copy(content = multiPart.mainBody ++ choice.content, label = label))
                  .toValidNec(s"No label defined for choice '${choice.name}' in selection '$name'")
                }
              .sequence
              .map(Selection(name, _))
                .toEither
                .leftMap(_.mkString_(", "))
          }
        }
    }
  }
  
  lazy val relativePath: Templates.Directive = Templates.create("relativePath") {
    import Templates.dsl._
    
    (attribute(0).as[Path], cursor).mapN { (path, cursor) =>
      TemplateString(path.relativeTo(cursor.path).toString)
    }
  }
  
  /** Implementation of the `format` directive for block elements in markup documents.
   *  The content of such a block will only be rendered for the corresponding
   *  output format (e.g. `pdf` or `html`).
   */
  lazy val format: Blocks.Directive = Blocks.eval("format") {
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

  /** Implementation of the `todo` directive for inline elements.
    */
  val todo: Spans.Directive = Spans.create("todo") {
    import Spans.dsl._
    attribute(0).map { _ => SpanSequence(Nil) }
  }

  /** Implementation of the `todo` directive for block elements.
    */
  val todoBlock: Blocks.Directive = Blocks.create("todo") {
    import Blocks.dsl._
    attribute(0).map { _ => BlockSequence(Nil) }
  }
  
  private def renderLinks (cursor: DocumentCursor,
                           suffixFilter: String => Boolean, 
                           includes: NonEmptyChain[Path],
                           excludes: Seq[Path],
                           render: RelativePath => String): TemplateElement = {
    
    val preFiltered = cursor.root.target.staticDocuments.filter(p => p.suffix.exists(suffixFilter) && !excludes.exists(p.isSubPath))

    val included = includes.foldLeft((preFiltered, Seq.empty[Path])) { case ((candidates, acc), include) =>
      val (newIncludes, remaining) = candidates.partition(_.isSubPath(include))
      (remaining, acc ++ newIncludes)
    }._2

    val allLinks = included.map { staticPath =>
      val path = staticPath.relativeTo(cursor.path)
      render(path)
    }
    TemplateElement(RawContent(NonEmptySet.of("html","xhtml"), allLinks.mkString("\n    ")))
  }

  /** Template directive that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    * 
    * The optional include array attribute can be used to specify a sequence of directories or documents
    * based on Laika's virtual path to be included.
    * If omitted the entire input tree will be searched for CSS documents.
    * 
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val linkCSSDirective: Templates.Directive = Templates.create("linkCSS") {
    import Templates.dsl._
    (attribute("paths").as[Seq[Path]].optional.widen, cursor).mapN { (includes, cursor) =>
      val suffixFilter: String => Boolean = cursor.root.targetFormat match {
        case Some("epub.xhtml") => suffix: String => suffix == "epub.css" || suffix == "shared.css"
        case Some("html") => suffix: String => suffix.endsWith("css") && suffix != "epub.css" && suffix != "page.css"
        case _ => _ => false
      }
      val excludePaths: Seq[Path] = Seq(cursor.root.config.get[Path](LaikaKeys.site.apiPath).toOption.getOrElse(Root / "api"))
      val includePaths: NonEmptyChain[Path] = NonEmptyChain.fromSeq(includes.getOrElse(Nil)).getOrElse(NonEmptyChain.one(Root))
      renderLinks(cursor, suffixFilter, includePaths, excludePaths, path => s"""<link rel="stylesheet" type="text/css" href="$path" />""")
    }
  }

  /** Template directive that inserts links to all JavaScript inputs found in the document tree, using a path
    * relative to the currently processed document. 
    *
    * The optional include array attribute can be used to specify a sequence of directories or documents
    * based on Laika's virtual path to be included.
    * If omitted the entire input tree will be searched for CSS documents.
    * 
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val linkJSDirective: Templates.Directive = Templates.create("linkJS") {
    import Templates.dsl._
    (attribute("paths").as[Seq[Path]].optional.widen, cursor).mapN { (includes, cursor) =>
      val includePaths: NonEmptyChain[Path] = NonEmptyChain.fromSeq(includes.getOrElse(Nil)).getOrElse(NonEmptyChain.one(Root))
      val excludePaths: Seq[Path] = Seq(cursor.root.config.get[Path](LaikaKeys.site.apiPath).toOption.getOrElse(Root / "api"))
      renderLinks(cursor, _ == "js", includePaths, excludePaths, path => s"""<script src="$path"></script>""")
    }
  }

  /** Markup directive for inserting an image as a block level element.
    *
    * The image source must be set with a positional attribute (in parenthesis). 
    * It is the only required attribute and can be a local path (absolute or relative) in the virtual tree
    * of your input sources, or an external URL (`http:` or `https:`).
    *
    * The optional `intrinsicWidth` and `intrinsicHeight` attributes can be used to describe the dimensions
    * of the image to avoid layout shifts when loading the page.
    *
    * For controlling the actual display size you can use the `style` attribute together with a matching declaration
    * in one of your site's CSS documents. 
    * If omitted the theme in use will usually have a sensible default size.
    *
    */
  val imageBlockDirective: Blocks.Directive = Blocks.create("image") {
    import Blocks.dsl._
    (attribute(0).as[String].widen,
      attribute("intrinsicWidth").as[Int].optional,
      attribute("intrinsicHeight").as[Int].optional,
      attribute("style").as[String].optional,
      attribute("alt").as[String].optional,
      attribute("title").as[String].optional,
      cursor).mapN { (src, width, height, style, alt, title, cursor) =>
        val img = Image(
          InternalTarget(PathBase.parse(src)).relativeTo(cursor.path),
          width.map(LengthUnit.px(_)),
          height.map(LengthUnit.px(_)),
          alt,
          title
        )
        val options = Styles(style.getOrElse("default-image-block"))
        BlockSequence(Seq(SpanSequence(img)), options = options)
      }
  }

  /** Markup directive for inserting an image as an inline element.
    *
    * The image source must be set with a positional attribute (in parenthesis). 
    * It is the only required attribute and can be a local path (absolute or relative) in the virtual tree
    * of your input sources, or an external URL (`http:` or `https:`).
    *
    * The optional `intrinsicWidth` and `intrinsicHeight` attributes can be used to describe the dimensions
    * of the image to avoid layout shifts when loading the page.
    *
    * For controlling the actual display size you can use the `style` attribute together with a matching declaration
    * in one of your site's CSS documents. 
    * If omitted the theme in use will usually have a sensible default size.
    *
    */
  val imageSpanDirective: Spans.Directive = Spans.create("image") {
    import Spans.dsl._
    (attribute(0).as[String].widen,
      attribute("intrinsicWidth").as[Int].optional,
      attribute("intrinsicHeight").as[Int].optional,
      attribute("style").as[String].optional,
      attribute("alt").as[String].optional,
      attribute("title").as[String].optional,
      cursor).mapN { (src, width, height, style, alt, title, cursor) =>
        val options = Styles(style.getOrElse("default-image-span"))
        Image(
          InternalTarget(PathBase.parse(src)).relativeTo(cursor.path),
          width.map(LengthUnit.px(_)),
          height.map(LengthUnit.px(_)),
          alt,
          title,
          options
        )
      }
  }

  /** The complete list of standard directives for block elements in markup documents.
    */
  lazy val blockDirectives: Seq[Blocks.Directive] = List(
    blockBreadcrumb,
    blockNav,
    blockFragment,
    blockStyle,
    imageBlockDirective,
    selectDirective,
    callout,
    format,
    pageBreak,
    todoBlock
  )

  /** The complete list of standard directives for span elements in markup documents.
    */
  lazy val spanDirectives: Seq[Spans.Directive] = List(
    imageSpanDirective,
    spanStyle,
    todo
  )
  
  /** The complete list of standard directives for templates.
   */
  lazy val templateDirectives: Seq[Templates.Directive] = List(
    templateBreadcrumb,
    templateNav,
    templateFor,
    templateIf,
    linkCSSDirective,
    linkJSDirective,
    relativePath
  )

  /** The complete list of standard directives for links.
    */
  lazy val linkDirectives: Seq[Links.Directive] = Seq(api, source)
  
}
