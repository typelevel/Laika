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
  
  /** The default rules for resolving link references 
   *  to be applied to the document.
   */
  def apply (cursor: DocumentCursor): RewriteRules = {
    
    val linkConfig = cursor.config.get[LinkConfig].getOrElse(LinkConfig.empty)
    val excludeFromValidation = linkConfig.excludeFromValidation.toSet
    val internalLinkMappings = linkConfig.internalLinkMappings
    
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

      def assignExternalUrl (link: Span, selector: PathSelector, target: ResolvedInternalTarget): Span = {
        internalLinkMappings.find(m => selector.path.isSubPath(m.internalPath))
          .fold(link) { mapping =>
            link match {
              case sl: SpanLink =>
                sl.copy(target = target.copy(
                  externalUrl = Some(mapping.externalBaseUrl + target.absolutePath.relativeTo(mapping.internalPath / "ref").toString)
                ))
              case _: Image =>
                InvalidSpan(s"image with internal path: ${target.absolutePath.toString} cannot be mapped to external base URL ${mapping.externalBaseUrl}", ref.source)
              case _ => link
            }
          }
      }
      
      def validateTarget (link: Span, selector: PathSelector, target: ResolvedInternalTarget): Span = {
        targets.select(Root, selector) match {
          case None if excludeFromValidation.exists(p => selector.path.isSubPath(p)) => link
          case None => InvalidSpan(s"unresolved internal reference: ${target.relativePath.toString}", ref.source)
          case Some(resolver) => cursor.target.targetFormats match {
            case TargetFormats.None => link // to be validated at point of inclusion by a directive like @:include
            case TargetFormats.All => resolver.targetFormats match {
              case TargetFormats.All => link
              case _ => InvalidSpan(s"document for all output formats cannot reference a document " +
                s"with restricted output formats: ${target.relativePath.toString}", ref.source)
            }
            case TargetFormats.Selected(formats) => 
              val missingFormats = formats.filterNot(resolver.targetFormats.includes)
              if (missingFormats.isEmpty) link
              else InvalidSpan(s"internal reference to: ${target.relativePath.toString} that does not support " +
                s"one or more output formats of this document: ${missingFormats.mkString(", ")}", ref.source)
          } 
            
        }
      } 
      
      def validateLink (link: Span, target: Target): Span = target match {
        case it: InternalTarget =>
          val resolvedTarget = it.relativeTo(cursor.path)
          val selector = pathSelectorFor(resolvedTarget.relativePath)
          val validated = validateTarget(link, selector, resolvedTarget)
          assignExternalUrl(validated, selector, resolvedTarget)
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
