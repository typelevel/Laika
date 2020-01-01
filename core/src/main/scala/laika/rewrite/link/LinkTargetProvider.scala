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
import IdGenerators._
import LinkTargets._

import scala.collection.mutable.ListBuffer

/** Provider for all tree elements that can be referenced from other
 *  elements, like images, footnotes, citations and other 
 *  inline targets. 
 * 
 *  @author Jens Halm
 */
class LinkTargetProvider (path: Path, root: RootElement) {

  private val headerIdMap = new IdMap
  
  /** Selects all elements from the document that can serve
   *  as a target for a reference element.
   */
  protected def selectTargets: List[TargetDefinition] = {
    
    val levels = new DecoratedHeaderLevels
    val symbols = new SymbolGenerator
    val numbers = new NumberGenerator
    val anonPos = Iterator.from(1)
                              
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
      
      case hd @ DecoratedHeader(_,_,Id(id)) => new DecoratedHeaderTarget(hd, suggestedId(id, headerIdMap), path, levels)
      
      case hd @ Header(_,_,Id(id))          => new HeaderTarget(hd, suggestedId(id, headerIdMap), path)
      
      case c: Customizable if c.options.id.isDefined => new CustomizableTarget(c, c.options.id.get, path)
    }
  }
  
  /** Resolves the specified targets, replacing elements
   *  with duplicate target ids with invalid block elements 
   *  and producing the ids of elements with generated identifiers. 
   */
  protected def resolveTargets (targets: Seq[TargetDefinition]): Seq[SingleTargetResolver] = {
    
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

    orderedTargets.foldLeft((new ListBuffer[SingleTargetResolver], usedIds, Set("id"))) {
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
    }._1.toSeq
  }
  
  /** Resolves all aliases contained in the specified target sequence,
   *  replacing them with the targets they are pointing to or with
   *  invalid block elements in case they cannot be resolved.
   */
  protected def resolveAliases (targets: Seq[SingleTargetResolver]): Seq[TargetResolver] = {

    val map = targets map (t => (t.selector, t)) toMap
    
    def resolve (alias: LinkAliasTarget, selector: Selector): SingleTargetResolver = {
      def doResolve (current: LinkAliasTarget, visited: Set[Any]): SingleTargetResolver = {
        if (visited.contains(current.id)) alias.invalid(s"circular link reference: ${alias.from}").withResolvedIds("","")
        else
          map.get(current.ref) map {
            case SingleTargetResolver(alias2: LinkAliasTarget, _, _, _) => doResolve(alias2, visited + current.id)
            case other => other.forAlias(selector)
          } getOrElse alias.invalid(s"unresolved link alias: ${alias.ref}").withResolvedIds("","")
      }  
      
      doResolve(alias, Set())
    }
                                   
    targets map { 
      case SingleTargetResolver(alias: LinkAliasTarget, selector, _, _) => resolve(alias, selector)
      case other => other 
    } 
    
  }
  
  /** Provides a map of all targets that can be referenced from elements
   *  within the same document.
   */
  val local: Map[Selector, TargetResolver] = resolveAliases(resolveTargets(selectTargets)).toList groupBy (_.selector) map { 
    case (selector: UniqueSelector, target :: Nil) => (selector, target)
    case (selector, list) => (selector, TargetSequenceResolver(list, selector))
  }
  
  /** Provides a map of all targets that can be referenced from elements
   *  within any document within the document tree.
   */
  val global: Map[Selector, TargetResolver] = {
    val global = local filter (_._2.global) 
    global ++ (global collect {
      case (UniqueSelector(name), target) => (PathSelector(path, name), target)
    })
  }
  
  /** A function for resolving header ids, taking the desired header id as
   *  input and returning the actual resolved id which might be different
   *  in case multiple headers were attempting to use the same id.
   */
  def headerIds: String => Option[String] = headerIdMap.lookupFunction
  
}
