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

package laika.parse.rst

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import laika.tree.Elements._
import laika.parse.rst.Elements._
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/** 
 *  The default rewrite rules that get applied to the raw document tree after parsing
 *  reStructuredTextMarkup. The rules are responsible for resolving internal references
 *  to link targets, footnotes, citations, substitution definitions and text roles.
 * 
 *  These rules are specific to `reStructuredText`, but some of them might get promoted
 *  to the general rules implemented in [[laika.tree.RewriteRules]] in a later release.
 *  
 *  @author Jens Halm
 */
object RewriteRules {

  
  private def invalidSpan (message: String, fallback: String) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
      
  private def selectSubstitutions (document: Document) = document.select { 
      case _: SubstitutionDefinition => true 
      case _ => false 
    } map { 
      case SubstitutionDefinition(id,content) => (id,content) 
    } toMap
    
  private def selectTextRoles (document: Document) = document.select { 
      case _: CustomizedTextRole => true
      case _ => false 
    } map { 
      case CustomizedTextRole(id,f) => (id,f)                                   
    } toMap  
    
  private def selectCitations (document: Document) = document.select { 
      case _: Citation => true 
      case _ => false 
    } map { 
      case Citation(label,content) => label 
    } toSet 
   
    
  private class FootnoteResolver (unresolvedFootnotes: List[Element], unresolvedReferences: List[Element]) {
     
    case class Key(key: AnyRef) {
      val hashed = key.hashCode
      override def equals(any: Any) = any match {
        case Key(other) => key eq other
        case _          => false
      }
      override def hashCode = hashed
    }

    class SymbolProvider {
      private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
      private val stream = Stream.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }.iterator
      def next = {
        val (sym,num) = stream.next
        sym.head.toString * num
      }
    }  
    
    object NumberProvider {
      private val explicitNumbers = unresolvedFootnotes.collect { case n @ Footnote(NumericLabel(num),_) => num } toSet 
      private val numberIt = Stream.from(1).iterator
      private val usedNums = new scala.collection.mutable.HashSet[Int]
      private lazy val usedIt = TreeSet(usedNums.toList:_*).iterator
      @tailrec final def next: Int = {
        val candidate = numberIt.next
        if (!explicitNumbers.contains(candidate)) { usedNums += candidate; candidate }
        else next
      }
      def remove (num: Int) = usedNums -= num
      def nextUsed = if (usedIt.hasNext) Some(usedIt.next.toString) else None
    }
    
    val symbols = new SymbolProvider
    
    private val resolvedFootnotes = (unresolvedFootnotes map { 
      case f @ Footnote(label,content) => 
        val (id, display) = label match {
          case Autonumber => 
            val num = NumberProvider.next.toString
            (num,num)
          case NumericLabel(num) => 
            val label = num.toString
            (label,label)
          case AutonumberLabel(label) => 
            (label,NumberProvider.next.toString)
          case Autosymbol =>
            val sym = symbols.next
            (sym,sym)
        }
        (Key(f), Footnote(ResolvedFootnoteLabel(id,display), content))
    })
    
    private val resolvedByIdentity = resolvedFootnotes toMap
    
    private val resolvedByLabel = (resolvedFootnotes map {
      case (_, fn @ Footnote(ResolvedFootnoteLabel(id,label), _)) => List((id -> fn),(label -> fn))
    }).flatten.toMap 
    
    val refSymbols = new SymbolProvider
    def toRef (fn: Footnote) = FootnoteReference(fn.label)
    def numberedLabelRef (fn: Footnote) = fn match {
      case Footnote(ResolvedFootnoteLabel(_,label), _) =>
        NumberProvider.remove(label.toInt)
        toRef(fn)
    }
    
    private val (autonumberedLabels, otherRefs) = unresolvedReferences partition {
      case FootnoteReference(AutonumberLabel(_)) => true; case _ => false
    }
    
    def resolve (id: String, f: Footnote => FootnoteReference, fallback: String, msg: String) = {
      resolvedByLabel.get(id).map(f).getOrElse(invalidSpan(msg, fallback))
    }
    
    private val resolvedReferences = (autonumberedLabels map {
      case r @ FootnoteReference(AutonumberLabel(label)) =>
        (Key(r), resolve(label, numberedLabelRef, "[#"+label+"]_", "unresolved footnote reference: " + label))
    }).toMap ++ (otherRefs map {
      case r @ FootnoteReference(Autonumber) => 
        val num = NumberProvider.nextUsed
        (Key(r), (num map {n => resolve(n, toRef, "[#]_", "too many autonumer references")})
          .getOrElse(invalidSpan("too many autonumer references", "[#]_")))
        
      case r @ FootnoteReference(NumericLabel(num)) => 
        (Key(r), resolve(num.toString, toRef, "["+num+"]_", "unresolved footnote reference: " + num))
        
      case r @ FootnoteReference(Autosymbol) =>
        val sym = refSymbols.next
        (Key(r), resolve(sym, toRef, "[*]_", "too many autosymbol references"))
    }).toMap
    
    def resolved (footnote: Footnote) = resolvedByIdentity(Key(footnote))
    
    def resolved (ref: FootnoteReference) = resolvedReferences(Key(ref))
  }
  
  private def selectFootnotes (document: Document) = {
    new FootnoteResolver(document.select { 
      case _: Footnote => true 
      case _ => false 
    }, document.select { 
      case _: FootnoteReference => true 
      case _ => false 
    })
  }
  
  private case class InvalidLinkTarget (id: String, message: SystemMessage) extends LinkTarget
  
  private def selectLinkTargets (document: Document) = {
    val linkIds = Stream.from(1).iterator
    
    def getId (id: String) = if (id.isEmpty) linkIds.next else id
    
    val unresolvedLinkTargets = document.select { 
      case _: LinkTarget => true
      case _ => false 
    } map { 
      case lt: LinkTarget => (getId(lt.id),lt) 
    } toMap
    
    def resolveIndirectTarget (target: IndirectLinkTarget, visited: Set[Any]): LinkTarget = {
      if (visited.contains(target.id)) 
        InvalidLinkTarget(target.id, SystemMessage(laika.tree.Elements.Error, "circular link reference: " + target.id))
      else
        unresolvedLinkTargets.get(target.ref.id) map {
          case it @ IndirectLinkTarget(id, _) => resolveIndirectTarget(it, visited + target.id)
          case other => other
        } getOrElse InvalidLinkTarget(target.id, SystemMessage(laika.tree.Elements.Error, "unresolved link reference: " + target.ref.id))
    }            
                                   
    unresolvedLinkTargets map { 
      case (id, it: IndirectLinkTarget) => (id, resolveIndirectTarget(it, Set()))
      case other => other 
    }    
  }
  
  
  /** Function providing the default rewrite rules when passed a document instance.
   */
  val defaults: Document => PartialFunction[Element, Option[Element]] = { document =>
    
    val substitutions = selectSubstitutions(document)
    
    val textRoles = selectTextRoles(document)
                                 
    val citations = selectCitations(document)             
                               
    val footnotes = selectFootnotes(document)
    
    val linkTargets = selectLinkTargets(document)
    val linkRefIds = Stream.from(1).iterator
    def getRefId (id: String) = if (id.isEmpty) linkRefIds.next else id
    
    // TODO - handle duplicate link target ids
    
    val levelMap = scala.collection.mutable.Map.empty[(Char,Boolean),Int]
    val levelIt = Stream.from(1).iterator
    
    val pf: PartialFunction[Element, Option[Element]] = {
      case SectionHeader(char, overline, content) => 
        Some(Header(levelMap.getOrElseUpdate((char,overline), levelIt.next), content))
        
      case SubstitutionReference(id) =>
        substitutions.get(id).orElse(Some(invalidSpan("unknown substitution id: " + id, "|"+id+"|")))
        
      case InterpretedText(role,text) =>
        textRoles.get(role).orElse(Some({s: String => invalidSpan("unknown text role: " + role, "`"+s+"`")})).map(_(text))
        
      case c @ CitationReference(label) =>
        Some(if (citations.contains(label)) c else invalidSpan("unresolved citation reference: " + label, "["+label+"]_"))
        
      case f @ Footnote(ResolvedFootnoteLabel(_,_),_)   => Some(f) // TODO - should not be required  
      case f @ Footnote(_,_)        => Some(footnotes.resolved(f))  
      case r @ FootnoteReference(_) => Some(footnotes.resolved(r))  
        
      case ref: LinkReference   => Some(linkTargets.get(getRefId(ref.id)) match {
        case Some(ExternalLinkTarget(id, url, title)) => Link(ref.content, url, title)
        case Some(InternalLinkTarget(id))         => Link(ref.content, "#"+id)
        case Some(InvalidLinkTarget(id, msg))     => 
          InvalidSpan(msg, SpanSequence(Text(ref.inputPrefix) :: ref.content.toList ::: Text(ref.inputPostfix) :: Nil))
        case None                                 =>
          val msg = if (ref.id.isEmpty) "too many anonymous link references" else "unresolved link reference: " + ref.id
          InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), 
              SpanSequence(Text(ref.inputPrefix) :: ref.content.toList ::: Text(ref.inputPostfix) :: Nil))
      })
      
      case _: SubstitutionDefinition   => None
      case _: LinkTarget               => None
      case _: CustomizedTextRole       => None
    }
    pf
  }

  /** Applies the default rewrite rules to the specified document tree,
   *  returning a new rewritten tree instance.
   */
  def applyDefaults (doc: Document) = doc.rewrite(defaults(doc)) 
  
   
  
}