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

package laika.rewrite.link

import laika.api.bundle.BlockDirectives
import laika.ast.Path.Root
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.ast._
import laika.api.config.Config.ConfigResult
import laika.parse.GeneratedSource

import scala.annotation.tailrec

/** The default rewrite rules responsible for resolving link references that get
  *  applied to the raw document tree after parsing.
  *
  *  These rules resolve references to images, footnotes, citations and other
  *  inline targets, and generate the identifiers for targets with auto-generated
  *  ids like auto-number footnotes.
  *
  *  The rules replace references pointing to internal or external targets
  *  with the corresponding resolved link elements, as well as the targets
  *  themselves with nodes that contain their final ids.
  *
  *  In case of duplicate target ids or unresolvable references runtime messages
  *  get inserted into the final document tree.
  *
  *  @author Jens Halm
  */
private[laika] class LinkResolver(root: DocumentTreeRoot, slugBuilder: String => String)
    extends RewriteRulesBuilder {

  val targets = new TreeTargets(root, slugBuilder)

  /** The default rules for resolving link references to be applied to the document.
    */
  def apply(cursor: DocumentCursor): ConfigResult[RewriteRules] = {

    val validator = new LinkValidator(
      cursor,
      path => targets.select(Root, PathSelector(path)).map(_.targetFormats)
    )

    def replace(element: Element, selector: Selector): Option[Element] =
      targets.select(cursor.path, selector)
        .flatMap(_.replaceTarget(element))

    def describeUnknownTarget(target: Element): String = {
      val id  = target.options.id.fold("<no id>")(id => s"id $id")
      val tpe = target.productPrefix
      s"link target of type $tpe with $id can only be inserted in RewritePhase.Build"
    }

    def replaceBlock(block: Block, selector: Selector): RewriteAction[Block] =
      replace(block, selector) match {
        case Some(b: Block) => Replace(b)
        case _              =>
          Replace(
            InvalidBlock(describeUnknownTarget(block), GeneratedSource).copy(fallback =
              block.withoutId
            )
          )
      }

    def replaceSpan(span: Span, selector: Selector): RewriteAction[Span] =
      replace(span, selector) match {
        case Some(b: Span) => Replace(b)
        case _             =>
          Replace(
            InvalidSpan(describeUnknownTarget(span), GeneratedSource).copy(fallback =
              span.withoutId
            )
          )
      }

    def resolveWith(
        ref: Reference,
        target: Option[TargetResolver],
        msg: => String
    ): RewriteAction[Span] = {

      val resolvedTarget = target.flatMap(_.resolveReference(LinkSource(ref, cursor.path))) match {
        case Some(link: SpanLink) => validator.validateAndRecover(link, ref.source)
        case Some(img: Image)     => validator.validateAndRecover(img, ref.source)
        case Some(other)          => other
        case None                 =>
          ref match {
            case p: PathReference =>
              val target = ReferenceResolver.resolveTarget(p.path, cursor.path)
              validator.validateAndRecover(p.resolve(target), ref.source)
            case _                =>
              InvalidSpan(msg, ref.source)
          }
      }
      Replace(resolvedTarget)
    }

    def resolveLocal(ref: Reference, selector: Selector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, targets.select(cursor.path, selector), msg)

    def resolvePath(ref: Reference, path: VirtualPath, msg: => String): RewriteAction[Span] = {
      val selector = PathSelector(InternalTarget(path).relativeTo(cursor.path).absolutePath)
      resolveWith(ref, targets.select(Root, selector), msg)
    }

    def selectRecursive(selector: UniqueSelector): Option[TargetResolver] = {

      @tailrec def selectFromParent(
          treeCursor: TreeCursor,
          selector: UniqueSelector
      ): Option[TargetResolver] = {
        val target = targets.select(treeCursor.path, selector)
        treeCursor.parent match {
          case Some(parent) if target.isEmpty => selectFromParent(parent, selector)
          case _                              => target
        }
      }
      targets.select(cursor.path, selector).orElse(selectFromParent(cursor.parent, selector))
    }

    def resolveId(ref: Reference, selector: UniqueSelector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, selectRecursive(selector), msg)

    def resolveIdOrSlug(ref: LinkIdReference, msg: => String): RewriteAction[Span] = {
      val target = selectRecursive(LinkDefinitionSelector(ref.ref))
        .orElse(selectRecursive(TargetIdSelector(slugBuilder(ref.ref))))
      resolveWith(ref, target, msg)
    }

    Right(RewriteRules.forBlocks {

      case f: FootnoteDefinition =>
        f.label match {
          case NumericLabel(num)   => replaceBlock(f, TargetIdSelector(num.toString))
          case AutonumberLabel(id) => replaceBlock(f, TargetIdSelector(slugBuilder(id)))
          case Autonumber          => replaceBlock(f, AutonumberSelector)
          case Autosymbol          => replaceBlock(f, AutosymbolSelector)
        }
      case c: Citation           => replaceBlock(c, TargetIdSelector(slugBuilder(c.label)))
      case h: DecoratedHeader    => replaceBlock(h, TargetIdSelector(slugBuilder(h.extractText)))
      case h: Header             => replaceBlock(h, TargetIdSelector(slugBuilder(h.extractText)))

      case d: BlockDirectives.DirectiveInstance if d.directive.exists(_.name == "fragment") =>
        Replace(d.resolve(cursor))

      case _: Hidden => Remove

      case elem if elem.hasId =>
        replaceBlock(elem, TargetIdSelector(slugBuilder(elem.options.id.get)))

    } ++ RewriteRules.forSpans {

      case c @ CitationReference(label, _, _) =>
        resolveLocal(
          c,
          TargetIdSelector(slugBuilder(label)),
          s"unresolved citation reference: $label"
        )

      case ref: FootnoteReference =>
        ref.label match {
          case NumericLabel(num)   =>
            resolveLocal(
              ref,
              TargetIdSelector(num.toString),
              s"unresolved footnote reference: $num"
            )
          case AutonumberLabel(id) =>
            resolveLocal(
              ref,
              TargetIdSelector(slugBuilder(id)),
              s"unresolved footnote reference: $id"
            )
          case Autonumber => resolveLocal(ref, AutonumberSelector, "too many autonumber references")
          case Autosymbol => resolveLocal(ref, AutosymbolSelector, "too many autosymbol references")
        }

      case ref: PathReference =>
        resolvePath(ref, ref.path, s"unresolved internal reference: ${ref.path.toString}")

      case ref: LinkIdReference =>
        if (ref.ref.isEmpty) resolveLocal(ref, AnonymousSelector, "too many anonymous references")
        else resolveIdOrSlug(ref, s"unresolved link id reference: ${ref.ref}")

      case ref: ImageIdReference =>
        resolveId(ref, LinkDefinitionSelector(ref.id), s"unresolved image reference: ${ref.id}")

      case elem if elem.hasId =>
        replaceSpan(elem, TargetIdSelector(slugBuilder(elem.options.id.get)))

      case _: Hidden => Remove

    })

  }

}
