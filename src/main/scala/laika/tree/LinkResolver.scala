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

  
  case object NamedLinkTarget
  case object AnonymousLinkTarget
  class InvalidTarget
  
  sealed abstract class Id
  case class Anonymous (pos: Int) extends Id
  case class Named (name: String) extends Id
  case class Generated (generator: Set[String] => String) extends Id
  case class Hybrid (id: String, display: Id) extends Id
  
  implicit def stringToId (name: String) = Named(name)
  
  case object Unresolved extends Element with Temporary
  case class DuplicateId (id: String) extends Element with Temporary
  case class CircularReference (id: String) extends Element with Temporary
  
  case class Target (group: AnyRef, id: Id, source: Element, resolved: Element = Unresolved) {
    val selector = id match {
      case Hybrid(id0,_) => Selector(group, id0)
      case other         => Selector(group, id)
    }
  }
    
  case class Selector (group: AnyRef, id: Id)
   
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
  
  def suggestedId (suggested: String): Id = Generated( used => {
    @tailrec def next (num: Int): String = {
      val candidate = suggested+"-"+num
      if (!used.contains(candidate)) candidate
      else next(num + 1)
    }
    if (!used.contains(suggested)) suggested else next(1)
  })
  

  
  class DefaultRules (val document: Document) { 
  
    def selectTargets = {
      val symbols = new SymbolGenerator
      val numbers = new NumberGenerator
      val anonPos = Stream.from(1).iterator
      def linkId (id: String) = if (id.isEmpty) (AnonymousLinkTarget, Anonymous(anonPos.next))
                                else (NamedLinkTarget, Named(id))
    
      document.collect {
        case c: Citation => Target(Citation, c.label, c)
        
        case f: FootnoteDefinition => f.label match {
          case Autosymbol            => Target(Autosymbol, Generated(symbols), f)
          case Autonumber            => Target(Autonumber, Generated(numbers), f)
          case AutonumberLabel(id)   => Target(AutonumberLabel, Hybrid(id, Generated(numbers)), f)
          case NumericLabel(num)     => Target(NumericLabel, num.toString, f)
        }
        
        case lt: ExternalLinkDefinition => val (group, id) = linkId(lt.id); Target(group, id, lt, lt)
        case lt: LinkAlias              => Target(NamedLinkTarget, lt.id, lt, lt)
        
        case hd @ DecoratedHeader(_,_,Id(id)) => Target(NamedLinkTarget, suggestedId(id), hd) // TODO - does not handle headers without id
        case hd @ Header(_,_,Id(id))          => Target(NamedLinkTarget, suggestedId(id), hd)
        
        case c: Customizable if c.options.id.isDefined => Target(NamedLinkTarget, c.options.id.get, c, c)
      }
    }
    
    def processDuplicateIds (name: String, targets: Seq[(Target,Int)]) = {
      
      def invalid (element: Element) = {
        val sysMsg = SystemMessage(Error, "duplicate target id: " + name)
        element match {
          case b: Block => InvalidBlock(sysMsg, TreeUtil.removeId(b))
          case s: Span  => InvalidSpan(sysMsg, TreeUtil.removeId(s))
          case _        => sysMsg
        }
      }
      
      val dup = DuplicateId(name)
      (targets map { 
        case (t @ Target(_, Named(name), FootnoteDefinition(_,content,opt), Unresolved), index) => 
          (t.copy(resolved = invalid(Footnote(name, content, opt)), group = new InvalidTarget), index)
        case (t @ Target(_, Hybrid(id,_), FootnoteDefinition(_,content,opt), Unresolved), index) => 
          (t.copy(resolved = invalid(Footnote(id, content, opt)), group = new InvalidTarget), index)
        case (t, index) => (t.copy(resolved = invalid(t.source), group = new InvalidTarget), index)
      }) ++
      ((targets groupBy (_._1.group)).keySet map { group => 
        (Target(group, name, dup, dup), -1)
      })
    }
      
    def resolveTargets (targets: Seq[Target]) = {
      
      val (ids, validatedTargets)  = (targets.zipWithIndex.groupBy {
        case (Target(_, Named(name), _, _),_)    => Some(name)
        case (Target(_, Hybrid(name,_), _, _),_) => Some(name)
        case _ => None
      } collect {
        case e @ (None,_) => e
        case (name, (target :: Nil)) => (name, target :: Nil)
        case (name, conflicting)   => (name, processDuplicateIds(name.get, conflicting))
      }).toSeq.unzip
      
      val usedIds = ids.filter(_ != None).map(_.get).toSet
      val orderedTargets = validatedTargets.flatten.sortBy(_._2).map(_._1)
      val processedTargets = ((new ListBuffer[Target], usedIds) /: orderedTargets) { 
        case ((buf, used), t) => t.id match {
          case Generated(f) => 
            val id = f(used)
            (buf += t.copy(id = id), used + id)
          case Hybrid(id, Generated(f)) =>
            val display = f(used)
            (buf += t.copy(id = Hybrid(id, display)), used + display)
          case _ => 
            (buf += t, used)
        }
      }._1
      
      val levelMap = scala.collection.mutable.Map.empty[HeaderDecoration,Int]
      val levelIt = Stream.from(1).iterator
      
      def targetId (id: String) = Id(id.replaceAll("[^a-zA-Z0-9]+","-").replaceFirst("^-","").replaceFirst("-$","")) // TODO - avoid duplicates
  
      processedTargets map {
        case t @ Target(_, Named(name), DecoratedHeader(deco, content, opt), Unresolved) => 
          t.copy(resolved = Header(levelMap.getOrElseUpdate(deco, levelIt.next), content, opt + targetId(name)))
        case t @ Target(_, Named(name), Header(level, content, opt), Unresolved) => 
          t.copy(resolved = Header(level, content, opt + targetId(name)))
        case t @ Target(_, Named(name), Citation(_,content,opt), Unresolved) => 
          t.copy(resolved = Citation(name, content, opt + Id(name)))
        case t @ Target(_, Named(name), FootnoteDefinition(_,content,opt), Unresolved) => 
          t.copy(resolved = Footnote(name, content, opt + Id(name)))
        case t @ Target(_, Hybrid(id,Named(display)), FootnoteDefinition(_,content,opt), Unresolved) => 
          t.copy(resolved = Footnote(display, content, opt + Id(id)))
        case t: Target => t
      }
    }
    
    def resolveAliases (targets: Map[Selector,Target]): Map[Selector,Target] = {
  
      def resolveAlias (alias: LinkAlias, visited: Set[Any]): Element = {
        if (visited.contains(alias.id)) CircularReference(alias.id)
        else
          targets.get(Selector(NamedLinkTarget, alias.target)) map {
            case Target(_,_, alias2: LinkAlias, _) => resolveAlias(alias2, visited + alias.id)
            case Target(_,_, other, _) => other
          } getOrElse Unresolved
      }            
                                     
      targets map { 
        case (selector, t @ Target(_,_, alias: LinkAlias, _)) => (selector, t.copy(resolved = resolveAlias(alias, Set())))
        case other => other 
      } 
    }
    
    case class Identity(ref: AnyRef) {
      val hashed = ref.hashCode
      override def equals(any: Any) = any match {
        case Identity(other) => ref eq other
        case _               => false
      }
      override def hashCode = hashed
    }
    
    
    val rewrite: PartialFunction[Element, Option[Element]] = {
      
      val targets = resolveTargets(selectTargets)
    
      val selectorMap = resolveAliases(targets map (t => (t.selector, t)) toMap)
      def byId (group: AnyRef, id: Id) = selectorMap.get(Selector(group, id)).map(_.resolved)
      
      val sourceMap = selectorMap.values map (t => (Identity(t.source), t.resolved)) toMap
      def bySource (source: Element) = sourceMap.get(Identity(source)).getOrElse(source)
    
      val groupMap = (selectorMap.values groupBy (_.group) mapValues (_.map(_.resolved).iterator)).view.force
  
      def byGroup (group: AnyRef) = { val it = groupMap.get(group); if (it.isDefined && it.get.hasNext) Some(it.get.next) else None }
    

      def invalid (target: Option[Element], source: String, msg: => String) = {
        def span (message: String, fallback: String) =
          InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
        target match {
          case Some(DuplicateId(id)) => Some(span("ambiguous reference to duplicate id: " + id, source))
          case Some(CircularReference(id)) => Some(span("circular link reference: " + id, source))
          case _ => Some(span(msg, source)) 
        }
      }
      
      def resolveFootnote (target: Option[Element], source: String, msg: => String) = target match {
        case Some(Footnote(label, _, Id(id))) => Some(FootnoteLink(id, label))
        case _ => invalid(target, source, msg)
      }
      
      {
        case f: FootnoteDefinition => Some(bySource(f)) 
        case c: Citation           => Some(bySource(c)) 
        case h: DecoratedHeader    => Some(bySource(h)) 
        case h: Header             => Some(bySource(h)) 
        
        case c @ CitationReference(label,source,opt) => byId(Citation, label) match {
          case Some(Citation(label, _, Id(id))) => Some(CitationLink(label,opt))
          case other => invalid(other, source, "unresolved citation reference: " + label)
        }
        
        case ref: FootnoteReference  => ref.label match {
          case NumericLabel(num) => 
            resolveFootnote(byId(NumericLabel, num.toString), ref.source, "unresolved footnote reference: " + num)
          case AutonumberLabel(id) => 
            resolveFootnote(byId(AutonumberLabel, id), ref.source, "unresolved footnote reference: " + id)
          case Autonumber => 
            resolveFootnote(byGroup(Autonumber), ref.source, "too many autonumber references")
          case Autosymbol => 
            resolveFootnote(byGroup(Autosymbol), ref.source, "too many autosymbol references")
        }
          
        case ref: LinkReference => val target = if (ref.id.isEmpty) byGroup(AnonymousLinkTarget) 
                                                else byId(NamedLinkTarget, ref.id)
          target match {
            case Some(ExternalLinkDefinition(_, url, title, _))  => Some(ExternalLink(ref.content, url, title, ref.options))
            case Some(c: Customizable) if c.options.id.isDefined => Some(InternalLink(ref.content, c.options.id.get, options = ref.options))
            case other =>
              val msg = if (ref.id.isEmpty) "too many anonymous link references" 
                        else "unresolved link reference: " + ref.id
              invalid(target, ref.source, msg)
          }
        
        case ref: ImageReference => byId(NamedLinkTarget, ref.id) match {
          case Some(ExternalLinkDefinition(_, url, title, _)) => Some(Image(ref.text, url, title, ref.options))
          case other => invalid(other, ref.source, "unresolved image reference: " + ref.id)
        }
          
        case _: Temporary => None
      }
    }
  
  }
  
  def apply (document: Document) = (new DefaultRules(document)).rewrite
  
}

