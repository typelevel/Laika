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
import LinkTargets._
import laika.config.Config

import scala.annotation.tailrec

/** Provider for all tree elements that can be referenced from other
 *  elements, like images, footnotes, citations and other 
 *  inline targets. 
 * 
 *  @author Jens Halm
 */
class LinkTargetProvider (path: Path, root: RootElement, config: Config) {

  /** Generates symbol identifiers. 
    *  Contains a predefined list of ten symbols to generate.
    *  If more than ten symbols are required, the same sequence 
    *  will be reused, doubled and then tripled, and so on ("**" etc.).
    */
  private class SymbolGenerator {
    private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
    private val stream = Iterator.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }
    final def next: String = {
      val (sym,num) = stream.next
      sym.head.toString * num
    }
  }
  private class DecoratedHeaderLevels {
    private val levelMap = scala.collection.mutable.Map.empty[HeaderDecoration,Int]
    private val levelIt = Iterator.from(1)
    def levelFor (deco: HeaderDecoration): Int = levelMap.getOrElseUpdate(deco, levelIt.next)
  }
  
  private def linkDefinitionResolver (selector: Selector, target: Target, title: Option[String] = None): TargetResolver = {
    
    def resolveTarget (refPath: Path): Target = target match {
      /* If an internal target point upwards beyond the virtual root of the processed tree, 
         it is treated as an external target and does not get validated. */
      case it: InternalTarget if it.relativePath.parentLevels >= path.depth => ExternalTarget(it.relativePath.toString)
      case it: InternalTarget => it.relativeTo(refPath)
      case external => external
    }
    val resolver = ReferenceResolver.lift {
      case LinkSource(LinkDefinitionReference (content, _, _, opt), sourcePath) =>
        SpanLink(content, resolveTarget(sourcePath.parent), title, opt)
      case LinkSource(ImageDefinitionReference (text, _, _, opt), sourcePath) =>
        Image(text, resolveTarget(sourcePath.parent), title = title, options = opt)
    }
    TargetResolver.create(selector, resolver, TargetReplacer.removeTarget)
  }
  
  private val directTargets: List[TargetResolver] = {
    
    val levels = new DecoratedHeaderLevels
    val symbols = new SymbolGenerator
    val symbolNumbers = Iterator.from(1)
    val numbers = Iterator.from(1)
    
    def internalResolver (selector: TargetIdSelector): LinkSource => Option[Span] =
      ReferenceResolver.internalLink(path.withFragment(selector.id))

    root.collect {
      case c: Citation =>
        val selector = TargetIdSelector(slug(c.label))
        val resolver = ReferenceResolver.lift {
          case LinkSource(CitationReference(label, _, opt), _) => CitationLink(selector.id, label, opt)
        }
        val replacer = TargetReplacer.lift {
          case Citation(label, content, opt) => Citation(label, content, opt + Id(s"__cit-${selector.id}"))
        }
        TargetResolver.create(selector, resolver, replacer)
      
      case f: FootnoteDefinition => 
        val (docId, displayId, selector) = f.label match {
          case Autosymbol            => (s"__fns-${symbolNumbers.next}", symbols.next, AutosymbolSelector) // TODO - move these prefix definitions somewhere else
          case Autonumber            => val num = numbers.next; (s"__fn-$num", num.toString, AutonumberSelector)
          case AutonumberLabel(id)   => (slug(id), numbers.next.toString, TargetIdSelector(slug(id)))
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

      case DecoratedHeader(_,_,Id(id)) => // TODO - do not generate id upfront
        val selector = TargetIdSelector(slug(id))
        val finalHeader = TargetReplacer.lift {
          case DecoratedHeader(deco, content, opt) => Header(levels.levelFor(deco), content, opt + Id(selector.id))
        }
        TargetResolver.create(selector, internalResolver(selector), finalHeader)
      
      case Header(_,_,Id(id)) => // TODO - do not generate id upfront
        val selector = TargetIdSelector(slug(id))
        TargetResolver.create(selector, internalResolver(selector), TargetReplacer.addId(selector.id))

      case alias: LinkAlias => 
        LinkAliasResolver.unresolved(TargetIdSelector(slug(alias.id)), TargetIdSelector(slug(alias.target)))  
        
      case c: Block if c.options.id.isDefined =>
        val selector = TargetIdSelector(slug(c.options.id.get))
        TargetResolver.create(selector, internalResolver(selector), TargetReplacer.addId(selector.id))

      case c: Span if c.options.id.isDefined =>
        val selector = TargetIdSelector(slug(c.options.id.get))
        TargetResolver.forSpanTarget(selector, internalResolver(selector))
    }
  }

  /** Provides a map of all targets that can be referenced from elements
    * within the same document.
    */
  val local: Map[Selector, TargetResolver] = {

    val groupedTargets: Map[Selector, TargetResolver] = directTargets.groupBy(_.selector).map {
      case (sel: UniqueSelector, target :: Nil) =>
        (sel, target)
      case (sel: UniqueSelector, targets) =>
        (sel, TargetResolver.forDuplicateSelector(sel, path, targets))
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
    
    val linkConfig = config.getOpt[LinkConfig].toOption.flatten.getOrElse(LinkConfig.empty) // TODO - 0.15 - error handling
    val targetsFromConfig = linkConfig.targets.map { defn =>
      val selector = LinkDefinitionSelector(defn.id)
      (selector, linkDefinitionResolver(selector, defn.target))
    }
    
    groupedTargets.map { 
      case (sel, alias: LinkAliasResolver) => (sel, resolve(alias, alias.targetSelector, Set()))
      case resolved => resolved
    } ++ targetsFromConfig
  }

  /** Provides a map of all targets that can be referenced from elements
   *  within any document within the document tree.
   */
  val global: Map[Selector, TargetResolver] = {
    val global = local filter (_._2.selector.global)
    val documentTarget =
      TargetResolver.create(PathSelector(path), ReferenceResolver.internalLink(path), TargetReplacer.removeTarget)
    (global ++ global.collect {
      case (TargetIdSelector(name), target) => (PathSelector(path.withFragment(name)), target)
    }) + ((documentTarget.selector, documentTarget))
  }
  
}
