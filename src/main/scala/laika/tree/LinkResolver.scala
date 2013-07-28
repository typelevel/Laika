/*
 * Copyright 2013 the original author or authors.
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

import laika.tree.Elements._

/**
 *  @author Jens Halm
 */
object LinkResolver extends (Document => PartialFunction[Element,Option[Element]]) {

  
  sealed abstract class Id
  case class Anonymous (pos: Int) extends Id
  case class Named (name: String) extends Id
  case class Generated (generator: Set[String] => String) extends Id
  case class Hybrid (id: String, display: Id) extends Id
  case object Hidden extends Id
  
  implicit def stringToId (name: String) = Named(name)
  
  sealed abstract class Selector
  case class UniqueSelector (name: String) extends Selector
  case object AnonymousSelector extends Selector
  case object AutonumberSelector extends Selector
  case object AutosymbolSelector extends Selector
  
  implicit def stringToSelector (name: String) = UniqueSelector(name)
  
  
  abstract class TargetDefinition (val source: Element, val id: Id) {
    
    def withResolvedIds (documentId: String, renderedId: String): UniqueResolvedTarget
    
    def invalid (msg: String) = new InvalidTarget(this, msg)

    protected def lift (f: PartialFunction[(Element,Id), Element]) = f lift
    
    def replace: ((Element,Id)) => Option[Element]
    
    def resolve: ((Element,Id)) => Option[Element]
    
  }
  
  class CitationTarget (citation: Citation) extends TargetDefinition(citation, citation.label) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, citation.label, documentId)
    val replace = lift { case (Citation(label,content,opt),    Named(id)) => Citation(label, content, opt + Id(id)) }
    val resolve = lift { case (CitationReference(label,_,opt), Named(id)) => CitationLink(id, label, opt) }
  }
  
  class FootnoteTarget (defn: FootnoteDefinition, id: Id, selector: Selector) extends TargetDefinition(defn, id) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, selector, Hybrid(documentId, Named(displayId)))
    val replace = lift { 
      case (FootnoteDefinition(_,content,opt), Hybrid(name,Named(display))) => Footnote(display, content, opt + Id(name))
    }
    val resolve = lift { 
      case (FootnoteReference (_, _, opt), Hybrid(id,Named(display))) => FootnoteLink(id, display, opt)
    }
    override def invalid (msg: String) = new InvalidTarget(this, msg) {
      override val replace = lift {
        case (FootnoteDefinition(_,content,opt), Named(name))                 => InvalidBlock(sysMsg, Footnote(name, content, opt))
        case (FootnoteDefinition(_,content,opt), Hybrid(name,Named(display))) => InvalidBlock(sysMsg, Footnote(display, content, opt))
      }
    }
  }
  
  class ExternalLinkTarget (definition: ExternalLinkDefinition, id: Id, selector: Selector) extends TargetDefinition(definition, id) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, selector, Hidden)
    val replace = lift (Map.empty) // TODO - use PartialFunction.empty when moving to 2.10
    val resolve = lift { 
      case (LinkReference (content, _, _, opt), _) => ExternalLink(content, definition.url, definition.title, opt)
      case (ImageReference (text, _, _, opt), _)   => Image(text, definition.url, definition.title, opt)
    }
  }
  
  class LinkAliasTarget (alias: LinkAlias) extends TargetDefinition(alias, alias.id) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, alias.id, Hidden)
    val replace = lift (Map.empty) // TODO - use PartialFunction.empty when moving to 2.10
    val resolve = lift (Map.empty)
    val ref = alias.target
    val from = alias.id
  }
  
  class InvalidTarget (delegate: TargetDefinition, msg: String) extends TargetDefinition(delegate.source, delegate.id) {
    def withResolvedIds (documentId: String, displayId: String) = {
      val t = delegate.withResolvedIds(documentId, displayId)
      new UniqueResolvedTarget(this, t.selector, t.render)
    }
    val sysMsg = SystemMessage(Error, msg)
    val replace = lift {
      case (target, id) =>
        val replaced = delegate.replace(target, id)
        replaced match {
          case Some(b: Block) => InvalidBlock(sysMsg, TreeUtil.removeId(b))
          case Some(s: Span)  => InvalidSpan(sysMsg, TreeUtil.removeId(s))
          case _              => sysMsg
        }
      }
    val resolve = lift {
      case (ref: Reference, _) => InvalidSpan(sysMsg, Text(ref.source))
    }
  }
  
  abstract class DefaultTarget (target: Customizable, id: Id) extends TargetDefinition(target, id) {
    val replace = lift { case (c: Customizable, Named(name))                    => TreeUtil.setId(c, name) }
    val resolve = lift { case (LinkReference (content, _, _, opt), Named(name)) => InternalLink(content, name, options = opt) } 
  }
  
  class CustomizableTarget (target: Customizable, id: String) extends DefaultTarget(target, id) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, id, documentId)
  }
  
  class HeaderTarget (header: Block, id: Id) extends DefaultTarget(header, id) {
    override def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, documentId, displayId)
  }
  
  class DecoratedHeaderTarget (header: DecoratedHeader, id: Id, levels: DecoratedHeaderLevels) extends HeaderTarget(header, id) {
    override val replace = lift  
      { case (DecoratedHeader(deco, content, opt), Named(name)) => Header(levels.levelFor(deco), content, opt + Id(name)) }
  }
    
  class DecoratedHeaderLevels {
    private val levelMap = scala.collection.mutable.Map.empty[HeaderDecoration,Int]
    private val levelIt = Stream.from(1).iterator
    def levelFor (deco: HeaderDecoration) = levelMap.getOrElseUpdate(deco, levelIt.next)
  }
  
  class IdMap {
    private val map = scala.collection.mutable.Map[String,ListBuffer[String]]()
    def += (origId: String, docId: String)= {
      val list = map.getOrElseUpdate(origId, ListBuffer())
      list += docId
    }
    def lookupFunction: String => Option[String] = {
      val lookup = map.mapValues(_.iterator).view.force
      s => lookup.get(s).flatMap(i => if (i.hasNext) Some(i.next) else None)
    }
  }
    
  abstract sealed class ResolvedTarget {
    
    def selector: Selector
    
    def resolveReference (rewrittenRef: Element): Option[Element]
    
    def replaceTarget (rewrittenOriginal: Element): Option[Element]
    
  }
  
  case class UniqueResolvedTarget (target: TargetDefinition, selector: Selector, render: Id) extends ResolvedTarget {
    
    def resolveReference (rewrittenRef: Element) = target.resolve(rewrittenRef, render)
    
    def replaceTarget (rewrittenOriginal: Element) = target.replace(rewrittenOriginal, render)
    
    def forAlias (newSelector: Selector) = new UniqueResolvedTarget(target, newSelector, render) {
      override def replaceTarget (rewrittenOriginal: Element) = None
    }
    
  }
  
  case class ResolvedTargetSequence (targets: Seq[ResolvedTarget], selector: Selector) extends ResolvedTarget {
    private val refIt = targets.iterator
    private val targetIt = targets.iterator
    def nextOption (it: Iterator[ResolvedTarget]) = if (it.hasNext) Some(it.next) else None
    
    def resolveReference (rewrittenRef: Element)   = nextOption(refIt).flatMap(_.resolveReference(rewrittenRef))
    def replaceTarget (rewrittenOriginal: Element) = nextOption(targetIt).flatMap(_.replaceTarget(rewrittenOriginal))
  }
  
  class SymbolGenerator extends (Set[String] => String) {
    private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
    private val stream = Stream.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }.iterator
    @tailrec final def apply (used: Set[String]) = {
      val (sym,num) = stream.next
      val candidate = sym.head.toString * num
      if (!used.contains(candidate)) candidate
      else apply(used)
    }
  } 
    
  class NumberGenerator extends (Set[String] => String) {
    private val numbers = Stream.from(1).iterator
    @tailrec final def apply (used: Set[String]) = {
      val candidate = numbers.next.toString
      if (!used.contains(candidate)) candidate 
      else apply(used)
    }
  }
  
  def suggestedId (suggested: String, map: IdMap) = Generated( used => {
    def result (str: String) = {
      map += (suggested, str)
      str
    }
    @tailrec def next (num: Int): String = {
      val candidate = suggested+"-"+num
      if (!used.contains(candidate)) result(candidate)
      else next(num + 1)
    }
    if (!used.contains(suggested)) result(suggested) else next(1)
  })
  
  
  class DefaultRules (document: Document) { 
  
    val headerIdMap = new IdMap
    val decHeaderIdMap = new IdMap
    
    def selectTargets = {
      
      val levels = new DecoratedHeaderLevels
      val symbols = new SymbolGenerator
      val numbers = new NumberGenerator
      val anonPos = Stream.from(1).iterator
                                
      document.collect {
        case c: Citation => new CitationTarget(c) 
        
        case f: FootnoteDefinition => f.label match {
          case Autosymbol            => new FootnoteTarget(f, Generated(symbols), AutosymbolSelector)
          case Autonumber            => new FootnoteTarget(f, Generated(numbers), AutonumberSelector)
          case AutonumberLabel(id)   => new FootnoteTarget(f, Hybrid(id, Generated(numbers)), id)
          case NumericLabel(num)     => new FootnoteTarget(f, Named(num.toString), num.toString)
        }
        
        case lt: ExternalLinkDefinition => if (lt.id.isEmpty) new ExternalLinkTarget(lt, Anonymous(anonPos.next), AnonymousSelector) 
                                           else               new ExternalLinkTarget(lt, Named(lt.id), lt.id)
        
        case lt: LinkAlias              => new LinkAliasTarget(lt)
        
        case hd @ DecoratedHeader(_,_,Id(id)) => new DecoratedHeaderTarget(hd, suggestedId(id, decHeaderIdMap), levels)  // TODO - does not handle headers without id
        
        case hd @ Header(_,_,Id(id))          => new HeaderTarget(hd, suggestedId(id, headerIdMap))
        
        case c: Customizable if c.options.id.isDefined => new CustomizableTarget(c, c.options.id.get)
      }
    }
    
    def resolveTargets (targets: Seq[TargetDefinition]) = {
      
      val docIdMap = new IdMap
      
      val (ids, validatedTargets)  = (targets.zipWithIndex.groupBy { t => t._1.id match {
        case Named(name)    => Some(name)
        case Hybrid(name,_) => Some(name)
        case _              => None
      }} collect {
        case e @ (None,_) => e
        case (name, (target :: Nil)) => (name, target :: Nil)
        case (name, conflicting)   => (name, conflicting map { case (t, index) => (t.invalid("duplicate target id: " + name.get), index) })
      }).toSeq.unzip
      
      val usedIds = ids.filter(_.isDefined).map(_.get).toSet
      val orderedTargets = validatedTargets.flatten.sortBy(_._2).map(_._1)
      
      def documentId (id: String, used: Set[String]) = {
        val suggest = id.replaceAll("[^a-zA-Z0-9-]+","-").replaceFirst("^-","").replaceFirst("-$","").toLowerCase
        val gen = suggestedId(if (suggest.isEmpty) "id" else suggest, docIdMap)
        gen.generator(used)
      }
      
      ((new ListBuffer[UniqueResolvedTarget], usedIds, Set("id")) /: orderedTargets) { 
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
    
    def resolveAliases (targets: Seq[UniqueResolvedTarget]): Seq[ResolvedTarget] = {
  
      val map = targets map (t => (t.selector, t)) toMap
      
      def resolve (alias: LinkAliasTarget, selector: Selector): UniqueResolvedTarget = {
        def doResolve (current: LinkAliasTarget, visited: Set[Any]): UniqueResolvedTarget = {
          if (visited.contains(current.id)) alias.invalid("circular link reference: " + alias.from).withResolvedIds("","")
          else
            map.get(current.ref) map {
              case UniqueResolvedTarget(alias2: LinkAliasTarget, _, _) => doResolve(alias2, visited + current.id)
              case other => other.forAlias(selector)
            } getOrElse alias.invalid("unresolved link alias: " + alias.ref).withResolvedIds("","")
        }  
        
        doResolve(alias, Set())
      }
                                     
      targets map { 
        case UniqueResolvedTarget(alias: LinkAliasTarget, selector, _) => resolve(alias, selector)
        case other => other 
      } 
      
    }
    
    val rewrite: PartialFunction[Element, Option[Element]] = {
      
      val resolvedTargets = resolveAliases(resolveTargets(selectTargets)).toList
          
      val headerId = headerIdMap.lookupFunction
      val decHeaderId = decHeaderIdMap.lookupFunction
      
      val targetMap = resolvedTargets groupBy (_.selector) map { 
        case (selector: UniqueSelector, target :: Nil) => (selector,target)
        case (selector, list) => (selector, new ResolvedTargetSequence(list,selector))
      }
      
      def replaceHeader (h: Block, origId: String, lookup: String => Option[String]) = lookup(origId).flatMap(replace(h,_))
      
      def replace (element: Element, selector: Selector) = 
        targetMap.get(selector).flatMap(_.replaceTarget(element))
        
      def resolve (ref: Reference, selector: Selector, msg: => String) = 
        Some(targetMap.get(selector).flatMap(_.resolveReference(ref)).getOrElse(InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Text(ref.source))))
      
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
        
        case c @ CitationReference(label,_,_) => resolve(c, label, "unresolved citation reference: " + label)
        
        case ref: FootnoteReference => ref.label match {
          case NumericLabel(num)   => resolve(ref, num.toString, "unresolved footnote reference: " + num)
          case AutonumberLabel(id) => resolve(ref, id, "unresolved footnote reference: " + id)
          case Autonumber          => resolve(ref, AutonumberSelector, "too many autonumber references")
          case Autosymbol          => resolve(ref, AutosymbolSelector, "too many autosymbol references")
        }
          
        case ref: LinkReference => if (ref.id.isEmpty) resolve(ref, AnonymousSelector, "too many anonymous link references")
                                   else                resolve(ref, ref.id, "unresolved link reference: " + ref.id)
          
        case ref: ImageReference => resolve(ref, ref.id, "unresolved image reference: " + ref.id)
         
        case _: Temporary => None

        case c: Customizable if c.options.id.isDefined => replace(c, c.options.id.get)
        
      }
    }
  
  }
  
  def apply (document: Document) = (new DefaultRules(document)).rewrite
  
}

