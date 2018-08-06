/*
 * Copyright 2013-2016 the original author or authors.
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
object LinkResolver extends (DocumentCursor => RewriteRule) {

  /** The default rules for resolving link references 
   *  to be applied to the document.
   */
  def apply (cursor: DocumentCursor): RewriteRule = {
    
    val targets = cursor.target.linkTargets
    val headerId = targets.headerIds
    
    def replaceHeader (h: Block, origId: String, lookup: String => Option[String]): Option[Element] = lookup(origId).flatMap(replace(h,_))
    
    def replace (element: Element, selector: Selector): Option[Element] = 
      targets.local.get(selector).flatMap(_.replaceTarget(element))
    
    def resolve (ref: Reference, selector: Selector, msg: => String, global: Boolean = false): Option[Element] = {
      
      def selectFromParent = {
        @tailrec def select (path: Path): (Option[TargetResolver],Option[Path]) = {
          val tree = cursor.root.target.selectSubtree(path)
          val target = tree.flatMap(_.selectTarget(selector))
          if (target.isDefined || path.parent == path) (target,Some(cursor.target.path))
          else select(path.parent)
        }
        val path = cursor.parent.target.path
        select(Path(Path.Current, path.components))
      }
      def selectFromRoot (path: String, name: String) = 
        (cursor.root.target.selectTarget(PathSelector(cursor.parent.target.path / Path(path), name)),Some(cursor.target.path))
      
      val (target, path) = {
        val local = targets.local.get(selector)
        if (local.isDefined) (local, None)
        else (selector, global) match {
          case (UniqueSelector(targetName), true) =>
            val index = targetName.indexOf(":")
            if (index == -1) selectFromParent
            else selectFromRoot(targetName take index, targetName drop (index+1))
          case _ => (None,None)
        }
      }
      Some(target.flatMap(_.resolveReference(ref,path))
          .getOrElse(InvalidElement(msg, ref.source).asSpan))
    }
      
    {
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replace(f, num.toString)
        case AutonumberLabel(id) => replace(f, id)
        case Autonumber          => replace(f, AutonumberSelector)
        case Autosymbol          => replace(f, AutosymbolSelector)
      }
      case c: Citation           => replace(c, c.label)
      case h: DecoratedHeader    => replaceHeader(h, h.options.id.get, headerId)
      case h@ Header(_,_,Id(id)) => replaceHeader(h, id, headerId)
      
      case c @ CitationReference(label,_,_) => resolve(c, label, s"unresolved citation reference: $label")
      
      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolve(ref, num.toString, s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolve(ref, id, s"unresolved footnote reference: $id")
        case Autonumber          => resolve(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolve(ref, AutosymbolSelector, "too many autosymbol references")
      }
        
      case ref: LinkReference => if (ref.id.isEmpty) resolve(ref, AnonymousSelector, "too many anonymous link references")
                                 else                resolve(ref, ref.id, s"unresolved link reference: ${ref.id}", global = true)
        
      case ref: ImageReference => resolve(ref, ref.id, s"unresolved image reference: ${ref.id}", global = true)
      case img @ Image(_,URI(uri, None),_,_,_,_) => Some(img.copy(uri = URI(uri, PathInfo.fromURI(uri, cursor.parent.target.path))))
      
      case _: Temporary => None

      case c: Customizable if c.options.id.isDefined => replace(c, c.options.id.get)
      
    }
  }
  
}
