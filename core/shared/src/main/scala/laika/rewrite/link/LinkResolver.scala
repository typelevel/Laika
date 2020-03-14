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
    
    def replace (element: Block, selector: Selector): RewriteAction[Block] =
      targets.local.get(selector)
        .flatMap(_.replaceTarget(element))
        .fold[RewriteAction[Block]](Remove)(Replace(_))

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
      resolveWith(ref, cursor.root.target.tree.selectTarget(selector), msg)
    }
    
    def resolveRecursive (ref: Reference, selector: UniqueSelector, msg: => String): RewriteAction[Span] = {
      
      @tailrec def selectFromParent (treeCursor: TreeCursor): Option[TargetResolver] = {
        val target = treeCursor.target.selectTarget(selector)
        treeCursor.parent match {
          case Some(parent) if target.isEmpty => selectFromParent(parent)
          case _ => target
        }
      }
    
      resolveWith(ref, targets.local.get(selector).orElse(selectFromParent(cursor.parent)), msg)
    }
      
    RewriteRules.forBlocks {
      
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replace(f, TargetIdSelector(num.toString))
        case AutonumberLabel(id) => replace(f, TargetIdSelector(id))
        case Autonumber          => replace(f, AutonumberSelector)
        case Autosymbol          => replace(f, AutosymbolSelector)
      }
      case c: Citation           => replace(c, TargetIdSelector(c.label))
      case h: DecoratedHeader    => replace(h, TargetIdSelector(slug(h.options.id.get)))
      case h@ Header(_,_,Id(id)) => replace(h, TargetIdSelector(slug(id)))
      
      case _: Temporary => Remove

      case c: Customizable if c.options.id.isDefined => replace(c, TargetIdSelector(c.options.id.get))
      
    } ++ RewriteRules.forSpans {
      
      case c @ CitationReference(label,_,_) => resolveLocal(c, TargetIdSelector(label), s"unresolved citation reference: $label")

      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolveLocal(ref, TargetIdSelector(num.toString), s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolveLocal(ref, TargetIdSelector(id), s"unresolved footnote reference: $id")
        case Autonumber          => resolveLocal(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolveLocal(ref, AutosymbolSelector, "too many autosymbol references")
      }

      case img @ Image(_,URI(uri, None),_,_,_,_) => Replace(img.copy(uri = URI(uri, LinkPath.fromURI(uri, cursor.parent.target.path))))

      case ref: InternalReference => resolveGlobal(ref, ref.path, s"unresolved internal reference: ${ref.path.toString}")  
        
      case ref: LinkDefinitionReference => if (ref.id.isEmpty) resolveLocal(ref, AnonymousSelector, "too many anonymous link references")
                                           else resolveRecursive(ref, LinkDefinitionSelector(ref.id), s"unresolved link reference: ${ref.id}")

      case ref: ImageDefinitionReference => resolveRecursive(ref, LinkDefinitionSelector(ref.id), s"unresolved image reference: ${ref.id}")

      case _: Temporary => Remove

    }
  }
  
}
