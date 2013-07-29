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

import IdGenerators._
import Elements._

/**
 *  @author Jens Halm
 */
object LinkTargets {

  
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
    override def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, displayId, documentId)
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
  
  
}