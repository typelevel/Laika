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
import LinkTargets._

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
 *  In case of duplicate target ids or unresolvable references system messages
 *  get inserted into the final document tree.
 * 
 *  @author Jens Halm
 */
object LinkResolver extends (DocumentCursor => RewriteRules) {

  /** The default rules for resolving link references 
   *  to be applied to the document.
   */
  def apply (cursor: DocumentCursor): RewriteRules = {
    
    val targets = cursor.target.linkTargets
    
    def replace (element: Customizable, selector: Selector): Option[Customizable] =
      targets.local.get(selector)
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

    def resolveWith (ref: Reference, target: Option[TargetResolver], msg: => String): RewriteAction[Span] = {
      val resolvedTarget = target.flatMap(_.resolveReference(LinkSource(ref, cursor.path)))
      Replace(resolvedTarget.getOrElse(InvalidElement(msg, ref.source).asSpan))
    }
    
    def resolveLocal (ref: Reference, selector: Selector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, targets.local.get(selector), msg)
    
    def resolveGlobal (ref: Reference, path: RelativePath, msg: => String): RewriteAction[Span] = {
      val selector = 
        if (path.name.isEmpty && path.fragment.nonEmpty) PathSelector(cursor.path.withFragment(path.fragment.get))
        else PathSelector(cursor.parent.target.path / path)
      resolveWith(ref, cursor.root.target.selectTarget(selector), msg)
    }

    def selectRecursive (selector: UniqueSelector): Option[TargetResolver] = {

      @tailrec def selectFromParent (treeCursor: TreeCursor, selector: UniqueSelector): Option[TargetResolver] = {
        val target = treeCursor.target.selectTarget(selector)
        treeCursor.parent match {
          case Some(parent) if target.isEmpty => selectFromParent(parent, selector)
          case _ => target
        }
      }
      targets.local.get(selector).orElse(selectFromParent(cursor.parent, selector))
    }
    
    def resolveRecursive (ref: Reference, selector: UniqueSelector, msg: => String): RewriteAction[Span] =
      resolveWith(ref, selectRecursive(selector), msg)
        
    def resolveGeneric (gen: GenericReference, msg: => String): RewriteAction[Span] =
      selectRecursive(LinkDefinitionSelector(gen.ref)) match {
        case None => 
          val slugRef = slug(gen.ref)
          resolveLocal(gen.copy(ref = slugRef).asInternalReference, TargetIdSelector(slugRef), msg)
        case some => 
          resolveWith(gen.asLinkDefinitionReference, some, msg)
      }
      
    RewriteRules.forBlocks {
      
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replaceBlock(f, TargetIdSelector(num.toString))
        case AutonumberLabel(id) => replaceBlock(f, TargetIdSelector(slug(id)))
        case Autonumber          => replaceBlock(f, AutonumberSelector)
        case Autosymbol          => replaceBlock(f, AutosymbolSelector)
      }
      case c: Citation           => replaceBlock(c, TargetIdSelector(slug(c.label)))
      case h: DecoratedHeader    => replaceBlock(h, TargetIdSelector(slug(h.options.id.get)))
      case h@ Header(_,_,Id(id)) => replaceBlock(h, TargetIdSelector(slug(id)))
      
      case _: Temporary => Remove

      case c: Customizable if c.options.id.isDefined => replaceBlock(c, TargetIdSelector(slug(c.options.id.get)))
      
    } ++ RewriteRules.forSpans {
      
      case c @ CitationReference(label,_,_) => resolveLocal(c, TargetIdSelector(slug(label)), s"unresolved citation reference: $label")

      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolveLocal(ref, TargetIdSelector(num.toString), s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolveLocal(ref, TargetIdSelector(slug(id)), s"unresolved footnote reference: $id")
        case Autonumber          => resolveLocal(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolveLocal(ref, AutosymbolSelector, "too many autosymbol references")
      }

      case img @ Image(_,URI(uri, None),_,_,_,_) => Replace(img.copy(uri = URI(uri, LinkPath.fromURI(uri, cursor.parent.target.path))))

      case ref: InternalReference => resolveGlobal(ref, ref.path, s"unresolved internal reference: ${ref.path.toString}")  
        
      case ref: LinkDefinitionReference => if (ref.id.isEmpty) resolveLocal(ref, AnonymousSelector, "too many anonymous link references")
                                           else resolveRecursive(ref, LinkDefinitionSelector(ref.id), s"unresolved link reference: ${ref.id}")

      case gen: GenericReference => if (gen.ref.isEmpty) resolveLocal(gen.asLinkDefinitionReference, AnonymousSelector, "too many anonymous references")
                                    else resolveGeneric(gen, s"unresolved reference: ${gen.ref}")

      case ref: ImageDefinitionReference => resolveRecursive(ref, LinkDefinitionSelector(ref.id), s"unresolved image reference: ${ref.id}")

      case c: Customizable if c.options.id.isDefined => replaceSpan(c, TargetIdSelector(slug(c.options.id.get)))
        
      case _: Temporary => Remove

    }
  }
  
}
