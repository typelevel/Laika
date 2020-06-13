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

import laika.ast._
import laika.ast.Path.Root

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
    
    val excludeFromValidation = cursor.config.getOpt[LinkConfig].toOption.flatten.getOrElse(LinkConfig.empty).excludeFromValidation.toSet
    
    def replace (element: Customizable, selector: Selector): Option[Customizable] = 
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
    
    def validateLink (link: SpanLink, ref: Reference): Span = link.target match {
      case InternalTarget(_, relativePath) =>
        val selector = pathSelectorFor(relativePath)
        if (excludeFromValidation.exists(p => selector.path.isSubPath(p)) || targets.select(Root, selector).isDefined) link
        else InvalidElement(s"unresolved internal reference: ${relativePath.toString}", ref.source).asSpan
      case _ => link
    }

    def resolveWith (ref: Reference, target: Option[TargetResolver], msg: => String): RewriteAction[Span] = {
      val resolvedTarget = target.flatMap(_.resolveReference(LinkSource(ref, cursor.path))) match {
        case Some(link: SpanLink) => validateLink(link, ref)
        case Some(other)          => other
        case None                 => ref match {
          case PathReference(content, path, _, title, opt) => 
            val target = ReferenceResolver.resolveTarget(InternalTarget.fromPath(path, cursor.path), cursor.path)
            validateLink(SpanLink(content, target, title, opt), ref)
          case _ => InvalidElement(msg, ref.source).asSpan
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

      case c: Customizable if c.options.id.isDefined => replaceBlock(c, TargetIdSelector(slugBuilder(c.options.id.get)))
      
    } ++ RewriteRules.forSpans {
      
      case c @ CitationReference(label,_,_) => resolveLocal(c, TargetIdSelector(slugBuilder(label)), s"unresolved citation reference: $label")

      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolveLocal(ref, TargetIdSelector(num.toString), s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolveLocal(ref, TargetIdSelector(slugBuilder(id)), s"unresolved footnote reference: $id")
        case Autonumber          => resolveLocal(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolveLocal(ref, AutosymbolSelector, "too many autosymbol references")
      }

      case ref: PathReference if ref.path.parentLevels >= cursor.path.depth => 
        Replace(SpanLink(ref.content, ExternalTarget(ref.path.toString), ref.title, ref.options))  
      
      case ref: PathReference => resolvePath(ref, ref.path, s"unresolved internal reference: ${ref.path.toString}")  
        
      case ref: LinkIdReference => if (ref.ref.isEmpty) resolveLocal(ref, AnonymousSelector, "too many anonymous references")
                                    else resolveIdOrSlug(ref, s"unresolved link id reference: ${ref.ref}")

      case ref: ImageIdReference => resolveId(ref, LinkDefinitionSelector(ref.id), s"unresolved image reference: ${ref.id}")

      case c: Customizable if c.options.id.isDefined => replaceSpan(c, TargetIdSelector(slugBuilder(c.options.id.get)))
        
      case _: Hidden => Remove

    }
  }
  
}
