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

/** Representations for various types of link targets.
 * 
 *  @author Jens Halm
 */
object LinkTargets {

  /** Represents a selector used for matching reference
   *  nodes to target nodes. The selectors often differ
   *  from both, the ids rendered in the final document
   *  and the ids used for display.
   */
  sealed abstract class Selector
  
  /** A selector based on a unique string identifier.
   */
  case class UniqueSelector (name: String) extends Selector
  
  /** An anonymous selector (usually matched by position).
   */
  case object AnonymousSelector extends Selector
  
  /** An auto-number selector (usually matched by position).
   */
  case object AutonumberSelector extends Selector
  
  /** An auto-symbol selector (usually matched by position).
   */
  case object AutosymbolSelector extends Selector
  
  /** Converts the specified string to a Selector instance
   *  that represents a unique identifier.
   */
  implicit def stringToSelector (name: String) = UniqueSelector(name)
  
  
  /** The definition of a link target in the document tree, holding
   *  the element itself and its identifier. Three abstract methods
   *  have to be implemented by the concrete implementations.
   */
  abstract class TargetDefinition (val source: Element, val id: Id, val global: Boolean) {

    /** Converts this target to a final, resolved target based
     *  on the specified identifiers.
     * 
     *  @param documentId the id used as an identifier in the final, rendered output,
     *  only containing ASCII alphanumeric characters and optionally a dash to avoid
     *  identifiers which are illegal in the final output format (e.g. HTML, PDF)
     *  @param renderedId the id used to display to the user
     *  @return a final, resolved target based on the specified identifiers
     */
    def withResolvedIds (documentId: String, renderedId: String): UniqueResolvedTarget

    /** Converts this target to an invalid one with the 
     *  specified error message.
     */
    def invalid (msg: String) = new InvalidTarget(this, msg)

    /** Convenience method lifting the partial function to a plain function
     *  returning an Option result.
     */
    protected def lift (f: PartialFunction[(Element,Id), Element]) = f lift
    
    /** Converts the original target element to the final representation 
     *  (with its final, resolved identifiers)
     */
    def replace: ((Element,Id)) => Option[Element]
    
    /** Converts an element referencing this target to the final
     *  link element.
     */
    def resolve: ((Element,Id)) => Option[Element]
    
  }
  
  class CitationTarget (citation: Citation) extends TargetDefinition(citation, citation.label, false) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, citation.label, documentId)
    val replace = lift { case (Citation(label,content,opt),    Named(id)) => Citation(label, content, opt + Id(id)) }
    val resolve = lift { case (CitationReference(label,_,opt), Named(id)) => CitationLink(id, label, opt) }
  }
  
  class FootnoteTarget (defn: FootnoteDefinition, id: Id, selector: Selector) extends TargetDefinition(defn, id, false) {
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
  
  class ExternalLinkTarget (definition: ExternalLinkDefinition, id: Id, selector: Selector) extends TargetDefinition(definition, id, true) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, selector, Hidden)
    val replace = lift (Map.empty) // TODO - use PartialFunction.empty when moving to 2.10
    val resolve = lift { 
      case (LinkReference (content, _, _, opt), _) => ExternalLink(content, definition.url, definition.title, opt)
      case (ImageReference (text, _, _, opt), _)   => Image(text, definition.url, definition.title, opt)
    }
  }
  
  class LinkAliasTarget (alias: LinkAlias) extends TargetDefinition(alias, alias.id, false) {
    def withResolvedIds (documentId: String, displayId: String) = new UniqueResolvedTarget(this, alias.id, Hidden)
    val replace = lift (Map.empty) // TODO - 2.10 - use PartialFunction.empty when removing support for 2.9.x
    val resolve = lift (Map.empty)
    val ref = alias.target
    val from = alias.id
  }
  
  class InvalidTarget (delegate: TargetDefinition, msg: String) extends TargetDefinition(delegate.source, delegate.id, delegate.global) {
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
  
  abstract class DefaultTarget (target: Customizable, id: Id) extends TargetDefinition(target, id, true) {
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
  
  /** Represents a resolved target that has its final identifier generated
   *  (if necessary) and can be used to resolve matching reference nodes.
   */
  abstract sealed class ResolvedTarget {
    
    /** Indicates whether this target is global, so that
     *  it can get referenced from within other documents.
     */
    def global: Boolean
    
    /** The selector to use to identify reference nodes
     *  matching this target.
     */
    def selector: Selector

    /** Creates the final link element for the specified reference
     *  pointing to this target. In case this target does not know
     *  how to resolve the element it should return `None˚.
     * 
     *  @param rewrittenRef the original reference node in the raw document, potentially
     *  already rewritten in case any of its children got rewritten
     */
    def resolveReference (rewrittenRef: Element): Option[Element]

    /** Creates the final target element (with its final, resolved identifiers).
     * 
     *  @param rewrittenOriginal the original target node in the raw document, potentially
     *  already rewritten in case any of its children got rewritten
     */
    def replaceTarget (rewrittenOriginal: Element): Option[Element]
    
  }
  
  /** Represents a target that can be selected based on a unique identifier.
   */
  case class UniqueResolvedTarget (target: TargetDefinition, selector: Selector, render: Id) extends ResolvedTarget {
    
    def global = target.global
    
    def resolveReference (rewrittenRef: Element) = target.resolve(rewrittenRef, render)
    
    def replaceTarget (rewrittenOriginal: Element) = target.replace(rewrittenOriginal, render)
    
    def forAlias (newSelector: Selector) = new UniqueResolvedTarget(target, newSelector, render) {
      override def replaceTarget (rewrittenOriginal: Element) = None
    }
    
  }

  /** Represents a sequence of targets where matching reference nodes
   *  get determined by position. The `resolveReference` and `resolveTarget`
   *  methods can be invoked as many times as this sequence contains elements.
   */
  case class ResolvedTargetSequence (targets: Seq[ResolvedTarget], selector: Selector) extends ResolvedTarget {
    private val refIt = targets.iterator
    private val targetIt = targets.iterator
    val global = false
    
    def nextOption (it: Iterator[ResolvedTarget]) = if (it.hasNext) Some(it.next) else None
    
    def resolveReference (rewrittenRef: Element)   = nextOption(refIt).flatMap(_.resolveReference(rewrittenRef))
    def replaceTarget (rewrittenOriginal: Element) = nextOption(targetIt).flatMap(_.replaceTarget(rewrittenOriginal))
  }
  
  
}