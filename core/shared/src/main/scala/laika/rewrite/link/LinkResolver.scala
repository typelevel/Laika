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

import laika.ast.{InternalTarget, _}
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.config.LaikaKeys
import laika.rewrite.nav.TargetFormats

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
class LinkResolver (root: DocumentTreeRoot, slugBuilder: String => String) extends (DocumentCursor => RewriteRules) {

  val targets = new TreeTargets(root, slugBuilder)
  
  /** The default rules for resolving link references to be applied to the document.
   */
  def apply (cursor: DocumentCursor): RewriteRules = {
    
    val siteBaseURL = cursor.config.getOpt[String](LaikaKeys.siteBaseURL).toOption.flatten
    
    val excludedPaths = cursor.config.get[LinkConfig].getOrElse(LinkConfig.empty).excludeFromValidation.toSet
    def excludeFromValidation (path: Path): Boolean = {
      
      def hasExcludedFlag (path: RelativePath): Boolean = cursor.root.tree.target.selectSubtree(path) match {
        case Some(tree) => !tree.config.get[Boolean](LaikaKeys.validateLinks).getOrElse(true)
        case None if path == CurrentTree => false
        case _ => hasExcludedFlag(path.parent)
      }
      
      excludedPaths.exists(path.isSubPath) || hasExcludedFlag(path.relative)
    }
    
    def replace (element: Element, selector: Selector): Option[Element] = 
      targets.select(cursor.path, selector)
        .flatMap(_.replaceTarget(element))

    def replaceBlock (element: Block, selector: Selector): RewriteAction[Block] =
      replace(element, selector) match {
        case Some(b: Block) => Replace(b)
        case _              => Remove
      }

    def replaceSpan (element: Span, selector: Selector): RewriteAction[Span] =
      replace(element, selector) match {
        case Some(b: Span) => Replace(b)
        case _             => Remove
      }
    
    def pathSelectorFor (path: RelativePath): PathSelector = 
      if (path.name.isEmpty && path.fragment.nonEmpty) PathSelector(cursor.path.withFragment(path.fragment.get))
      else PathSelector(cursor.parent.target.path / path)
    
    def resolveWith (ref: Reference, target: Option[TargetResolver], msg: => String): RewriteAction[Span] = {

      def validateTarget (link: Span, selector: PathSelector, target: ResolvedInternalTarget): Span = {

        def attemptRecovery (internalFormats: TargetFormats, msg: => String): Span = {
          (internalFormats.includes("html"), siteBaseURL, link) match {
            case (true, Some(_), sp: SpanLink) => sp.copy(target = target.copy(
              internalFormats = internalFormats
            ))
            case _ => InvalidSpan(msg, ref.source)
          }
        }
        
        def validCondition: String = "unless html is one of the formats and siteBaseUrl is defined"
        def invalidRefMsg: String = s"cannot reference document '${target.relativePath.toString}'"
        targets.select(Root, selector) match {
          case None if excludeFromValidation(selector.path) => link
          case None => InvalidSpan(s"unresolved internal reference: ${target.relativePath.toString}", ref.source)
          case Some(resolver) => cursor.target.targetFormats match {
            case TargetFormats.None => link // to be validated at point of inclusion by a directive like @:include
            case TargetFormats.All => resolver.targetFormats match {
              case TargetFormats.All => link
              case TargetFormats.None => InvalidSpan(s"$invalidRefMsg as it is excluded from rendering", ref.source)
              case TargetFormats.Selected(_) => 
                def msg = s"document for all output formats $invalidRefMsg with restricted output formats $validCondition"
                attemptRecovery(resolver.targetFormats, msg)
            }
            case TargetFormats.Selected(formats) => 
              val missingFormats = formats.filterNot(resolver.targetFormats.includes)
              if (missingFormats.isEmpty) link
              else {
                def msg = s"$invalidRefMsg that does not support some of the formats of this document (${missingFormats.mkString(", ")}) $validCondition"
                attemptRecovery(resolver.targetFormats, msg)
              }
          }
        }
      } 
      
      def validateLink (link: Span, target: Target): Span = target match {
        case it: InternalTarget =>
          val resolvedTarget = it.relativeTo(cursor.path)
          val selector = pathSelectorFor(resolvedTarget.relativePath)
          validateTarget(link, selector, resolvedTarget)
        case _ => link
      }

      val resolvedTarget = target.flatMap(_.resolveReference(LinkSource(ref, cursor.path))) match {
        case Some(link: SpanLink) => validateLink(link, link.target)
        case Some(img: Image)     => validateLink(img, img.target)
        case Some(other)          => other
        case None                 => ref match {
          case p: PathReference =>
            val target = ReferenceResolver.resolveTarget(p.path, cursor.path)
            validateLink(p.resolve(target), target)
          case _ =>
            InvalidSpan(msg, ref.source)
        }
      }
      Replace(resolvedTarget)
    }
    
    def resolveLocal (ref: Reference, selector: Selector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, targets.select(cursor.path, selector), msg)
    
    def resolvePath (ref: Reference, path: RelativePath, msg: => String): RewriteAction[Span] = {
      val selector = pathSelectorFor(path)
      resolveWith(ref, targets.select(Root, selector), msg)
    }

    def selectRecursive (selector: UniqueSelector): Option[TargetResolver] = {

      @tailrec def selectFromParent (treeCursor: TreeCursor, selector: UniqueSelector): Option[TargetResolver] = {
        val target = targets.select(treeCursor.path, selector)
        treeCursor.parent match {
          case Some(parent) if target.isEmpty => selectFromParent(parent, selector)
          case _ => target
        }
      }
      targets.select(cursor.path, selector).orElse(selectFromParent(cursor.parent, selector))
    }
    
    def resolveId (ref: Reference, selector: UniqueSelector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, selectRecursive(selector), msg)
        
    def resolveIdOrSlug (ref: LinkIdReference, msg: => String): RewriteAction[Span] = {
      val target = selectRecursive(LinkDefinitionSelector(ref.ref))
        .orElse(selectRecursive(TargetIdSelector(slugBuilder(ref.ref))))
      resolveWith(ref, target, msg)
    }
      
    RewriteRules.forBlocks {
      
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replaceBlock(f, TargetIdSelector(num.toString))
        case AutonumberLabel(id) => replaceBlock(f, TargetIdSelector(slugBuilder(id)))
        case Autonumber          => replaceBlock(f, AutonumberSelector)
        case Autosymbol          => replaceBlock(f, AutosymbolSelector)
      }
      case c: Citation           => replaceBlock(c, TargetIdSelector(slugBuilder(c.label)))
      case h: DecoratedHeader    => replaceBlock(h, TargetIdSelector(slugBuilder(h.extractText)))
      case h: Header             => replaceBlock(h, TargetIdSelector(slugBuilder(h.extractText)))
      
      case _: Hidden => Remove

      case elem if elem.hasId    => replaceBlock(elem, TargetIdSelector(slugBuilder(elem.options.id.get)))
      
    } ++ RewriteRules.forSpans {
      
      case c @ CitationReference(label,_,_) => resolveLocal(c, TargetIdSelector(slugBuilder(label)), s"unresolved citation reference: $label")

      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolveLocal(ref, TargetIdSelector(num.toString), s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolveLocal(ref, TargetIdSelector(slugBuilder(id)), s"unresolved footnote reference: $id")
        case Autonumber          => resolveLocal(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolveLocal(ref, AutosymbolSelector, "too many autosymbol references")
      }

      case ref: PathReference    => resolvePath(ref, ref.path, s"unresolved internal reference: ${ref.path.toString}")

      case ref: LinkIdReference  => if (ref.ref.isEmpty) resolveLocal(ref, AnonymousSelector, "too many anonymous references")
                                    else resolveIdOrSlug(ref, s"unresolved link id reference: ${ref.ref}")

      case ref: ImageIdReference => resolveId(ref, LinkDefinitionSelector(ref.id), s"unresolved image reference: ${ref.id}")

      case elem if elem.hasId    => replaceSpan(elem, TargetIdSelector(slugBuilder(elem.options.id.get)))
        
      case _: Hidden => Remove

    }
  }
  
}
