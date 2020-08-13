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

import scala.annotation.tailrec

/** Collects all tree elements from a document that can be referenced from other elements, 
  * like images, footnotes, citations and other inline targets. 
 * 
 *  @author Jens Halm
 */
case class DocumentTargets (document: Document, slugBuilder: String => String) {

  /** Generates symbol identifiers. 
    * Contains a predefined list of ten symbols to generate.
    * If more than ten symbols are required, the same sequence 
    * will be reused, doubled and then tripled, and so on ("**" etc.).
    */
  private class SymbolGenerator {
    private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
    private val stream = Iterator.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }
    final def next(): String = {
      val (sym,num) = stream.next()
      sym.head.toString * num
    }
  }
  private class DecoratedHeaderLevels {
    private val levelMap = scala.collection.mutable.Map.empty[HeaderDecoration,Int]
    private val levelIt = Iterator.from(1)
    def levelFor (deco: HeaderDecoration): Int = levelMap.getOrElseUpdate(deco, levelIt.next())
  }
  
  private def linkDefinitionResolver (selector: Selector, target: Target, title: Option[String] = None): TargetResolver = {
    val resolver = ReferenceResolver.lift {
      case LinkSource(LinkIdReference(content, _, _, opt), sourcePath) =>
        SpanLink(content, ReferenceResolver.resolveTarget(target, sourcePath), title, opt)
      case LinkSource(ImageIdReference(text, _, _, opt), sourcePath) =>
        Image(ReferenceResolver.resolveTarget(target, sourcePath), alt = Some(text), title = title, options = opt)
    }
    TargetResolver.create(selector, resolver, TargetReplacer.removeTarget)
  }
  
  private val directTargets: List[TargetResolver] = {
    
    val levels = new DecoratedHeaderLevels
    val symbols = new SymbolGenerator
    val symbolNumbers = Iterator.from(1)
    val numbers = Iterator.from(1)
    
    def internalResolver (selector: TargetIdSelector): LinkSource => Option[Span] =
      ReferenceResolver.internalLink(document.path.withFragment(selector.id))

    document.content.collect {
      case c: Citation =>
        val selector = TargetIdSelector(slugBuilder(c.label))
        val resolver = ReferenceResolver.lift {
          case LinkSource(CitationReference(label, _, opt), _) => CitationLink(selector.id, label, opt)
        }
        val replacer = TargetReplacer.lift {
          case Citation(label, content, opt) => Citation(label, content, opt + Id(s"__cit-${selector.id}"))
        }
        TargetResolver.create(selector, resolver, replacer)
      
      case f: FootnoteDefinition => 
        val (docId, displayId, selector) = f.label match {
          case Autosymbol            => (s"__fns-${symbolNumbers.next()}", symbols.next(), AutosymbolSelector) // TODO - move these prefix definitions somewhere else
          case Autonumber            => val num = numbers.next(); (s"__fn-$num", num.toString, AutonumberSelector)
          case AutonumberLabel(id)   => (slugBuilder(id), numbers.next().toString, TargetIdSelector(slugBuilder(id)))
          case NumericLabel(num)     => (s"__fnl-$num", num.toString, TargetIdSelector(num.toString))
        }
        val resolver = ReferenceResolver.lift {
          case LinkSource(FootnoteReference(_, _, opt), _) => FootnoteLink(docId, displayId, opt)
        }
        val replacer = TargetReplacer.lift {
          case FootnoteDefinition(_, content, opt) => Footnote(displayId, content, opt + Id(docId))
        }
        TargetResolver.create(selector, resolver, replacer)

      case ld: LinkDefinition =>
        val selector = if (ld.id.isEmpty) AnonymousSelector else LinkDefinitionSelector(ld.id)
        linkDefinitionResolver(selector, ld.target, ld.title)

      case h @ DecoratedHeader(deco,_,_) =>
        val selector = TargetIdSelector(slugBuilder(h.extractText))
        val level = levels.levelFor(deco)
        val finalHeader = TargetReplacer.lift {
          case DecoratedHeader(_, content, opt) => Header(level, content, opt + Id(selector.id))
        }
        TargetResolver.create(selector, internalResolver(selector), finalHeader, 10 - level)
      
      case h @ Header(level,_,_) =>
        val selector = TargetIdSelector(slugBuilder(h.extractText))
        TargetResolver.create(selector, internalResolver(selector), TargetReplacer.addId(selector.id), 10 - level)

      case alias: LinkAlias => 
        LinkAliasResolver.unresolved(TargetIdSelector(slugBuilder(alias.id)), TargetIdSelector(slugBuilder(alias.target)))  
        
      case c: Block if c.options.id.isDefined =>
        val selector = TargetIdSelector(slugBuilder(c.options.id.get))
        TargetResolver.create(selector, internalResolver(selector), TargetReplacer.addId(selector.id))

      case c: Span if c.options.id.isDefined =>
        val selector = TargetIdSelector(slugBuilder(c.options.id.get))
        TargetResolver.forSpanTarget(selector, internalResolver(selector))
    }
  }

  /** Provides all targets that can be referenced inside the document.
    */
  val targets: Seq[TargetResolver] = {

    val groupedTargets: Map[Selector, TargetResolver] = directTargets.groupBy(_.selector).map {
      case (sel: UniqueSelector, target :: Nil) =>
        (sel, target)
      case (sel: UniqueSelector, duplicates) =>
        (sel, TargetResolver.forDuplicateSelector(sel, document.path, duplicates))
      case (selector, list) =>
        (selector, TargetSequenceResolver(list, selector))
    }
    
    @tailrec
    def resolve (alias: LinkAliasResolver, targetSelector: TargetIdSelector, visited: Set[TargetIdSelector]): TargetResolver = {
      if (visited.contains(alias.targetSelector)) alias.circularReference
      else groupedTargets.get(alias.targetSelector) match {
        case Some(alias2: LinkAliasResolver) => resolve(alias, alias2.targetSelector, visited + alias2.sourceSelector)
        case Some(resolved)                  => alias.resolveWith(resolved.resolveReference)
        case None                            => alias
      }
    }
    
    val linkConfig = document.config.get[LinkConfig].getOrElse(LinkConfig.empty) // TODO - 0.16 - error handling
    val targetsFromConfig = linkConfig.targets.map { defn =>
      linkDefinitionResolver(LinkDefinitionSelector(defn.id), defn.target)
    }
    
    val resolvedTargets = groupedTargets.toSeq.map { 
      case (_, alias: LinkAliasResolver) => resolve(alias, alias.targetSelector, Set())
      case (_, resolved)                 => resolved
    }
    
    val pathTargets = resolvedTargets.flatMap { target =>
      target.selector match {
        case TargetIdSelector(name) => Some(TargetResolver.forDelegate(PathSelector(document.path.withFragment(name)), target))
        case _ => None
      }
    }
  
    val documentTarget =
      TargetResolver.create(PathSelector(document.path), ReferenceResolver.internalLink(document.path), TargetReplacer.removeTarget)

    resolvedTargets ++ pathTargets ++ targetsFromConfig :+ documentTarget
  }
  
}
