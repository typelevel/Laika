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

package laika.tree

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import IdGenerators._
import LinkTargets._
import Elements._
import Documents._

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
class LinkResolver (path: Path, root: RootElement) {

  private val headerIdMap = new IdMap
  private val decHeaderIdMap = new IdMap
  
  /** Selects all elements from the document that can serve
   *  as a target for a reference element.
   */
  def selectTargets: List[TargetDefinition] = {
    
    val levels = new DecoratedHeaderLevels
    val symbols = new SymbolGenerator
    val numbers = new NumberGenerator
    val anonPos = Stream.from(1).iterator
                              
    root.collect {
      case c: Citation => new CitationTarget(c) 
      
      case f: FootnoteDefinition => f.label match {
        case Autosymbol            => new FootnoteTarget(f, Generated(symbols), AutosymbolSelector)
        case Autonumber            => new FootnoteTarget(f, Generated(numbers), AutonumberSelector)
        case AutonumberLabel(id)   => new FootnoteTarget(f, Hybrid(id, Generated(numbers)), id)
        case NumericLabel(num)     => new FootnoteTarget(f, Named(num.toString), num.toString)
      }
      
      case lt: ExternalLinkDefinition => if (lt.id.isEmpty) new ExternalLinkTarget(lt, Anonymous(anonPos.next), AnonymousSelector, path) 
                                         else               new ExternalLinkTarget(lt, Named(lt.id), lt.id, path)
      
      case lt: LinkAlias              => new LinkAliasTarget(lt)
      
      case hd @ DecoratedHeader(_,_,Id(id)) => new DecoratedHeaderTarget(hd, suggestedId(id, decHeaderIdMap), path, levels)  // TODO - does not handle headers without id
      
      case hd @ Header(_,_,Id(id))          => new HeaderTarget(hd, suggestedId(id, headerIdMap), path)
      
      case c: Customizable if c.options.id.isDefined => new CustomizableTarget(c, c.options.id.get, path)
    }
  }
  
  /** Resolves the specified targets, replacing elements
   *  with duplicate target ids with invalid block elements 
   *  and producing the ids of elements with generated identifiers. 
   */
  def resolveTargets (targets: Seq[TargetDefinition]): Seq[SingleTargetResolver] = {
    
    val docIdMap = new IdMap
    
    val (ids, validatedTargets)  = (targets.zipWithIndex.groupBy { t => t._1.id match {
      case Named(name)    => Some(name)
      case Hybrid(name,_) => Some(name)
      case _              => None
    }} collect {
      case e @ (None,_) => e
      case (name, (target :: Nil)) => (name, target :: Nil)
      case (name, conflicting)   => (name, conflicting map { case (t, index) => (t.invalid(s"duplicate target id: ${name.get}"), index) })
    }).toSeq.unzip
    
    val usedIds = ids.filter(_.isDefined).map(_.get).toSet
    val orderedTargets = validatedTargets.flatten.sortBy(_._2).map(_._1)
    
    def documentId (id: String, used: Set[String]) = {
      val suggest = id.replaceAll("[^a-zA-Z0-9-]+","-").replaceFirst("^-","").replaceFirst("-$","").toLowerCase
      val gen = suggestedId(if (suggest.isEmpty) "id" else suggest, docIdMap)
      gen.generator(used)
    }
    
    ((new ListBuffer[SingleTargetResolver], usedIds, Set("id")) /: orderedTargets) { 
      case ((buf, used, docIds), t) => t.id match {
        case Generated(f) => 
          val displayId = f(used)
          val docId = documentId(displayId, docIds)
          (buf += t.withResolvedIds(docId, displayId), used + displayId, docIds + docId)
        case Hybrid(id, Generated(f)) =>
          val display = f(used)
          val docId = documentId(id, docIds)
          (buf += t.withResolvedIds(docId, display), used + display, docIds + docId)
        case Named(name) =>
          val docId = documentId(name, docIds)
          (buf += t.withResolvedIds(docId, name), used, docIds + docId)
        case _ =>
          (buf += t.withResolvedIds("", ""), used, docIds)
      }
    }._1
  }
  
  /** Resolves all aliases contained in the specified target sequence,
   *  replacing them with the targets they are pointing to or with
   *  invalid block elements in case they cannot be resolved.
   */
  def resolveAliases (targets: Seq[SingleTargetResolver]): Seq[TargetResolver] = {

    val map = targets map (t => (t.selector, t)) toMap
    
    def resolve (alias: LinkAliasTarget, selector: Selector): SingleTargetResolver = {
      def doResolve (current: LinkAliasTarget, visited: Set[Any]): SingleTargetResolver = {
        if (visited.contains(current.id)) alias.invalid(s"circular link reference: ${alias.from}").withResolvedIds("","")
        else
          map.get(current.ref) map {
            case SingleTargetResolver(alias2: LinkAliasTarget, _, _) => doResolve(alias2, visited + current.id)
            case other => other.forAlias(selector)
          } getOrElse alias.invalid(s"unresolved link alias: ${alias.ref}").withResolvedIds("","")
      }  
      
      doResolve(alias, Set())
    }
                                   
    targets map { 
      case SingleTargetResolver(alias: LinkAliasTarget, selector, _) => resolve(alias, selector)
      case other => other 
    } 
    
  }
  
  val allTargets: Map[Selector, TargetResolver] = resolveAliases(resolveTargets(selectTargets)).toList groupBy (_.selector) map { 
    case (selector: UniqueSelector, target :: Nil) => (selector,target)
    case (selector, list) => (selector, new TargetSequenceResolver(list,selector))
  }
  
  val globalTargets: Map[Selector, TargetResolver] = allTargets filter (_._2.global)
  
  /** The default rules for resolving link references 
   *  to be applied to the document.
   */
  def rewriteRules (context: DocumentContext): RewriteRule = {
    
    val headerId = headerIdMap.lookupFunction
    val decHeaderId = decHeaderIdMap.lookupFunction
    
    def replaceHeader (h: Block, origId: String, lookup: String => Option[String]): Option[Element] = lookup(origId).flatMap(replace(h,_))
    
    def replace (element: Element, selector: Selector): Option[Element] = 
      allTargets.get(selector).flatMap(_.replaceTarget(element))
    
    def resolve (ref: Reference, selector: Selector, msg: => String, global: Boolean = false): Option[Element] = {
      
      def selectFromParent = {
        @tailrec def select (path: Path): (Option[TargetResolver],Option[Path]) = {
          val tree = context.root.selectSubtree(path)
          val target = tree.flatMap(_.selectTarget(selector))
          if (target.isDefined || path.parent == path) (target,Some(context.document.path))
          else select(path.parent)
        }
        val path = context.parent.path
        select(Path(Current, path.components))
      }
      def selectFromRoot (path: String, name: String) = 
        (context.root.selectTarget(PathSelector(context.parent.path / Path(path), name)),Some(context.document.path))
      
      val (target, path) = {
        val local = allTargets.get(selector)
        if (local.isDefined) (local, None)
        else (selector, global) match {
          case (UniqueSelector(targetName), true) => {
            val index = targetName.indexOf(":")
            if (index == -1) selectFromParent
            else selectFromRoot(targetName take index, targetName drop (index+1))
          }
          case _ => (None,None)
        }
      }
      Some(target.flatMap(_.resolveReference(ref,path))
          .getOrElse(InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Text(ref.source))))
    }
      
    {
      case f: FootnoteDefinition => f.label match {
        case NumericLabel(num)   => replace(f, num.toString)
        case AutonumberLabel(id) => replace(f, id)
        case Autonumber          => replace(f, AutonumberSelector)
        case Autosymbol          => replace(f, AutosymbolSelector)
      }
      case c: Citation           => replace(c, c.label)
      case h: DecoratedHeader    => replaceHeader(h, h.options.id.get, decHeaderId)
      case h@ Header(_,_,Id(id)) => replaceHeader(h, id, headerId)
      
      case c @ CitationReference(label,_,_) => resolve(c, label, s"unresolved citation reference: $label")
      
      case ref: FootnoteReference => ref.label match {
        case NumericLabel(num)   => resolve(ref, num.toString, s"unresolved footnote reference: $num")
        case AutonumberLabel(id) => resolve(ref, id, s"unresolved footnote reference: $id")
        case Autonumber          => resolve(ref, AutonumberSelector, "too many autonumber references")
        case Autosymbol          => resolve(ref, AutosymbolSelector, "too many autosymbol references")
      }
        
      case ref: LinkReference => if (ref.id.isEmpty) resolve(ref, AnonymousSelector, "too many anonymous link references")
                                 else                resolve(ref, ref.id, s"unresolved link reference: ${ref.id}", true)
        
      case ref: ImageReference => resolve(ref, ref.id, s"unresolved image reference: ${ref.id}", true)
      case img @ Image(_,URI(uri, None),_,_) => Some(img.copy(uri = URI(uri, PathInfo.fromURI(uri, context.parent.path)))) 
      
      case _: Temporary => None

      case c: Customizable if c.options.id.isDefined => replace(c, c.options.id.get)
      
    }
  }
}
  
object LinkResolver {
  
  /** Provides a link resolver
   *  for the specified root element (without executing it).
   */
  def apply (path: Path, root: RootElement): LinkResolver = new LinkResolver(path,root)
  
}

