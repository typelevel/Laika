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
    val headerId = targets.headerIds
    
    def replaceHeader (h: Block, origId: String, lookup: String => Option[String]): RewriteAction[Block] = 
      lookup(origId).fold[RewriteAction[Block]](Remove)(id => replace(h, UniqueSelector(id)))

    def replace (element: Block, selector: Selector): RewriteAction[Block] =
      targets.local.get(selector)
        .flatMap(_.replaceTarget(element))
        .collect{ case b: Block => b }
        .fold[RewriteAction[Block]](Remove)(Replace(_))

    def replaceSpan (element: Span, selector: Selector): RewriteAction[Span] =
      targets.local.get(selector)
        .flatMap(_.replaceTarget(element))
        .collect{ case s: Span => s }
        .fold[RewriteAction[Span]](Remove)(Replace(_))

    def selectFromRoot (selector: Selector): Option[TargetResolver] =
      cursor.root.target.tree.selectTarget(selector)

    def selectorFor (path: RelativePath): Selector = PathSelector(cursor.parent.target.path / path)
    
    def selectFromRootLegacy (path: String, name: String): Option[TargetResolver] = {
      val selector = selectorFor(RelativePath.parse(path).withFragment(name))
      selectFromRoot(selector)
    }
    
    def resolve (ref: Reference, selector: Selector, msg: => String, global: Boolean = false): RewriteAction[Span] = {
      
      def selectFromParent = {
        @tailrec def select (treeCursor: TreeCursor): Option[TargetResolver] = {
          val target = treeCursor.target.selectTarget(selector)
          treeCursor.parent match {
            case Some(parent) if target.isEmpty => select(parent)
            case _ => target
          }
        }
        select(cursor.parent)
      }
      
      val (target, path) = {
        val local = targets.local.get(selector)
        if (local.isDefined) (local, None)
        else (selector, global) match {
          case (sel: PathSelector, true) => (selectFromRoot(sel), Some(cursor.path))
          case (UniqueSelector(targetName), true) =>
            val index = targetName.indexOf(":")
            if (index == -1) (selectFromParent, Some(cursor.path))
            else (selectFromRootLegacy(targetName.take(index), targetName.drop(index+1)), Some(cursor.path))
          case _ => (None,None)
        }
      }
      Replace(target.flatMap(_.resolveReference(ref,path))
          .getOrElse(InvalidElement(msg, ref.source).asSpan))
    }
      
    RewriteRules.forBlocks {
      
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replace(f, UniqueSelector(num.toString))
        case AutonumberLabel(id) => replace(f, UniqueSelector(id))
        case Autonumber          => replace(f, AutonumberSelector)
        case Autosymbol          => replace(f, AutosymbolSelector)
      }
      case c: Citation           => replace(c, UniqueSelector(c.label))
      case h: DecoratedHeader    => replaceHeader(h, h.options.id.get, headerId)
      case h@ Header(_,_,Id(id)) => replaceHeader(h, id, headerId)
      
      case _: Temporary => Remove

      case c: Customizable if c.options.id.isDefined => replace(c, UniqueSelector(c.options.id.get))
      
    } ++ RewriteRules.forSpans {
      
      case c @ CitationReference(label,_,_) => resolve(c, UniqueSelector(label), s"unresolved citation reference: $label")

      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolve(ref, UniqueSelector(num.toString), s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolve(ref, UniqueSelector(id), s"unresolved footnote reference: $id")
        case Autonumber          => resolve(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolve(ref, AutosymbolSelector, "too many autosymbol references")
      }

      case img @ Image(_,URI(uri, None),_,_,_,_) => Replace(img.copy(uri = URI(uri, PathInfo.fromURI(uri, cursor.parent.target.path))))

      case ref: CrossReference => resolve(ref, selectorFor(ref.path), s"unresolved cross reference: ${ref.path.toString}", global = true)  
        
      case ref: LinkReference => if (ref.id.isEmpty) resolve(ref, AnonymousSelector, "too many anonymous link references")
                                 else                resolve(ref, UniqueSelector(ref.id), s"unresolved link reference: ${ref.id}", global = true)

      case ref: ImageReference => resolve(ref, UniqueSelector(ref.id), s"unresolved image reference: ${ref.id}", global = true)

      case _: Temporary => Remove

      case c: Customizable if c.options.id.isDefined => replaceSpan(c, UniqueSelector(c.options.id.get))

    }
  }
  
}
