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

class LinkTargetResolver {

  
  object NamedLinkTarget
  object AnonymousLinkTarget
  
  sealed abstract class Id
  case class Anonymous (pos: Int) extends Id
  case class Named (name: String) extends Id
  case class Generated (generator: Set[String] => String) extends Id
  case class Hybrid (id: Id, display: Id) extends Id
  
  implicit def stringToId (name: String) = Named(name)
  
  case object Unresolved extends Element with Temporary
  
  case class Target (group: AnyRef, id: Id, source: Element, resolved: Element = Unresolved) {
    val selector = Selector(group, id)  
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
  
  def suggestedId (suggested: String): Set[String] => String = { used => {
    @tailrec def next (num: Int): String = {
      val candidate = suggested+"-"+num
      if (!used.contains(candidate)) candidate
      else next(num + 1)
    }
    if (!used.contains(suggested)) suggested else next(1)
  }}
  
  def invalid (element: Element, msg: String) = {
    val sysMsg = SystemMessage(Error, msg)
    element match {
      case b: Block => InvalidBlock(sysMsg, b)
      case s: Span  => InvalidSpan(sysMsg, s)
      case _        => sysMsg
    }
  }
  
    
  private def selectTargets (document: Document) = {
    val symbols = new SymbolGenerator
    val numbers = new NumberGenerator
    val anonPos = Stream.from(1).iterator
    def linkId (id: String) = if (id.isEmpty) (AnonymousLinkTarget, Anonymous(anonPos.next))
                          else (NamedLinkTarget, Named(id))
  
    document.collect {
      case c: Citation                => Target(Citation, c.id, c, c)
      
      case f: FootnoteDefinition      => f.label match {
        case Autosymbol                 => Target(Autosymbol, Generated(symbols), f)
        case Autonumber                 => Target(Autonumber, Generated(numbers), f)
        case AutonumberLabel(id)        => Target(AutonumberLabel, Hybrid(id, Generated(numbers)), f)
        case NumericLabel(num)          => Target(NumericLabel, num.toString, f)
      }
      
      case lt: ExternalLinkDefinition => val (group, id) = linkId(lt.id); Target(group, id, lt, lt)
      case lt: InternalLinkTarget     => Target(NamedLinkTarget, lt.id, lt, lt)
      
      case c: Customizable if c.options.id.isDefined => Target(NamedLinkTarget, c.options.id.get, c, c)
    }
  }
    
  private def resolveTargets (targets: Seq[Target]) = {
    
    val (withExplicitId, withOtherId) = targets.partition {
      case Target(_, Named(_), _, _) => true
      case Target(_, Hybrid(Named(_),_), _, _) => true
      case _ => false
    }
    
    val (explicitIds, explicitTargets)  = (withExplicitId.groupBy {
      case Target(_, Named(name), _, _) => name
      case Target(_, Hybrid(Named(name),_), _, _) => name
      case _ => ""
    } map {
      case e @ (_, _ :: Nil)   => e
      case (name, conflicting) => (name, conflicting map {
        t => t.copy(resolved = invalid(t.source, "duplicate target id: " + name))
      })
    }).toSeq.unzip
    
    val otherTargets = ((new ListBuffer[Target], explicitIds.toSet) /: withOtherId) { 
      case ((buf, used), t) => t.id match {
        case Generated(f) => 
          val id = f(used)
          (buf += t.copy(id = id), used + id)
        case Hybrid(Generated(f), display) =>
          val id = f(used)
          (buf += t.copy(id = Hybrid(id, display)), used + id)
        case Hybrid(id, Generated(f)) =>
          val display = f(used)
          (buf += t.copy(id = Hybrid(id, display)), used + display)
        case _ => 
          (buf += t, used)
      }
    }._1
    
    (explicitTargets.flatten ++ otherTargets) map {
      case t @ Target(_, Named(name), FootnoteDefinition(_,content,opt), Unresolved) => 
        t.copy(resolved = Footnote(name, name, content, opt))
      case t @ Target(_, Hybrid(Named(id),Named(display)), FootnoteDefinition(_,content,opt), Unresolved) => 
        t.copy(resolved = Footnote(id, display, content, opt))
      case t: Target => t
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
  
  
  private def invalidSpan (message: String, fallback: String) =
    InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
  
  val rewriteRule: Document => PartialFunction[Element, Option[Element]] = { document =>
    
    val targets = resolveTargets(selectTargets(document))
  
    val sourceMap = targets map (t => (Identity(t.source), t.resolved)) toMap
    def bySource (source: Element) = sourceMap.get(Identity(source)).get
  
    val selectorMap = targets map (t => (t.selector, t.resolved)) toMap
    def byId (group: AnyRef, id: Id) = selectorMap.get(Selector(group, id))
    
    val groupMap = targets groupBy (_.group) mapValues (_.map(_.resolved).iterator)
    def byGroup (group: AnyRef) = { val it = groupMap(group); if (it.hasNext) Some(it.next) else None }
  
    def resolveFootnote (target: Option[Element], source: String, msg: => String) = {
       target match {
        case Some(Footnote(id, label, _, _)) => Some(FootnoteLink(id, label))
        case _ => Some(invalidSpan(msg, source)) 
      }
    }
    
    val pf: PartialFunction[Element, Option[Element]] = {
      
      case c @ CitationReference(label,source,_) =>
        Some(if (byId(Citation, label).isDefined) CitationLink(label) 
             else invalidSpan("unresolved citation reference: " + label, source))
        
      case f: Footnote           => Some(f) // TODO - should not be required  
      case f: FootnoteDefinition => Some(bySource(f)) 
      
      case ref: FootnoteReference  => ref.label match {
        case NumericLabel(num) => 
          resolveFootnote(byId(NumericLabel, num.toString), ref.source, "unresolved footnote reference: " + num)
        case AutonumberLabel(id) => 
          resolveFootnote(byId(AutonumberLabel, id), ref.source, "unresolved footnote reference: " + id)
        case Autonumber => 
          resolveFootnote(byGroup(Autonumber), ref.source, "too many autonumer references")
        case Autosymbol => 
          resolveFootnote(byGroup(Autosymbol), ref.source, "too many autosymbol references")
      }
        
      case ref: LinkReference => val target = if (ref.id.isEmpty) byGroup(AnonymousLinkTarget) 
                                              else byId(NamedLinkTarget, ref.id)
        target match {
          case Some(ExternalLinkDefinition(_, url, title, _)) => Some(ExternalLink(ref.content, url, title))
          case Some(InternalLinkTarget(id,_))                 => Some(InternalLink(ref.content, "#"+id))
          case other =>
            val msg = if (other.isEmpty && ref.id.isEmpty) "too many anonymous link references" 
                      else "unresolved link reference: " + ref.id
            Some(InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Text(ref.source)))
        }
      
      case _: Temporary => None
    }
    pf
  }
  
  
}