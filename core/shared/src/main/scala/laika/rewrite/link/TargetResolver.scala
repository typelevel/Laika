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
import laika.parse.GeneratedSource
import laika.rewrite.nav.TargetFormats


/** Represents the source of a link, its document path
  * and the actual inline span that is representing the link. 
  */
case class LinkSource (span: Span, path: Path)

/** Represents a resolver for a target that has its final identifier generated
  * (if necessary) and can be used to resolve matching reference nodes.
  *
  * @param selector the selector to use to identify reference nodes matching this target 
  * @param precedence the precedence in comparison to other resolvers with the same selector
  */
abstract sealed class TargetResolver (val selector: Selector, val targetFormats: TargetFormats = TargetFormats.All, val precedence: Int = 0) {

  /** Creates the final link element for the specified reference pointing to this target. 
    * In case this target does not know how to resolve the element it should return `None`.
    *
    * @param linkSource the source of the link
    */
  def resolveReference (linkSource: LinkSource): Option[Span]

  /** Creates the final target element (with its final, resolved identifiers).
    *
    * @param rewrittenOriginal the original target node in the raw document, potentially
    * already rewritten in case any of its children got rewritten
    */
  def replaceTarget (rewrittenOriginal: Element): Option[Element]

}

object ReferenceResolver {
  def lift(f: PartialFunction[LinkSource, Span]): LinkSource => Option[Span] = f.lift
  def internalLink (target: Path): LinkSource => Option[Span] = lift {
    case LinkSource(ref: PathReference, sourcePath) =>
      ref.resolve(InternalTarget(target).relativeTo(sourcePath))
    case LinkSource(LinkIdReference (content, _, _, opt), sourcePath) =>
      SpanLink(content, InternalTarget(target).relativeTo(sourcePath), None, opt)
  }
  def resolveTarget (target: RelativePath, refPath: Path): Target = {
    /* If an internal target point upwards beyond the virtual root of the processed tree, 
       it is treated as an external target and does not get validated. */
    if (target.parentLevels >= refPath.depth) ExternalTarget(target.toString)
    else InternalTarget(target).relativeTo(refPath)
  }
  def resolveTarget (target: Target, refPath: Path): Target = target match {
    case it: RelativeInternalTarget => resolveTarget(it.path, refPath)
    case it: InternalTarget => it.relativeTo(refPath)
    case external => external
  }
}

object TargetReplacer {
  def lift(f: PartialFunction[Block, Block]): Block => Option[Block] = f.lift
  def addId(id: String): Block => Option[Block] = block => Some(block.withId(id))
  val removeId: Block => Option[Block] = block => Some(block.withoutId)
  val removeTarget: Block => Option[Block] = Function.const(None)
}

object TargetResolver {

  def create(selector: Selector,
             referenceResolver: LinkSource => Option[Span],
             targetResolver: Block => Option[Block],
             targetFormats: TargetFormats = TargetFormats.All,
             precedence: Int = 0): TargetResolver = new TargetResolver(selector, targetFormats, precedence) {

    override def resolveReference (linkSource: LinkSource): Option[Span] = referenceResolver(linkSource)

    override def replaceTarget (rewrittenOriginal: Element): Option[Element] = rewrittenOriginal match {
      case b: Block => targetResolver(b)
      case _ => None
    }
  }

  def forSpanTarget(idSelector: TargetIdSelector,
                    referenceResolver: LinkSource => Option[Span],
                    targetFormats: TargetFormats = TargetFormats.All): TargetResolver = new TargetResolver(idSelector, targetFormats) {

    override def resolveReference (linkSource: LinkSource): Option[Span] = referenceResolver(linkSource)

    override def replaceTarget (rewrittenOriginal: Element): Option[Element] = rewrittenOriginal match {
      case s: Span => Some(s.withId(idSelector.id))
      case _ => None
    }
  }

  def forInvalidTarget (selector: UniqueSelector, msg: String): TargetResolver = new TargetResolver(selector) {
    val sysMsg: RuntimeMessage = RuntimeMessage(MessageLevel.Error, msg)
    val resolver = ReferenceResolver.lift { case LinkSource(ref: Reference, _) => InvalidSpan(msg, ref.source) }

    override def resolveReference (linkSource: LinkSource): Option[Span] = resolver(linkSource)

    override def replaceTarget (rewrittenOriginal: Element): Option[Element] = rewrittenOriginal match {
      case b: Block => Some(InvalidBlock(sysMsg, GeneratedSource, b.withoutId))
      case s: Span  => Some(InvalidSpan(sysMsg, GeneratedSource, s.withoutId))
      case _ => None
    }
  }

  def forDuplicateSelector (uniqueSelector: UniqueSelector,
                            path: Path,
                            targets: Seq[TargetResolver],
                            isDocScope: Boolean): TargetResolver = new TargetResolver(uniqueSelector) {

    private val sorted = targets.sortBy(_.precedence).reverse
    private val resolver: LinkSource => Option[Span] = sorted.takeWhile(_.precedence == sorted.head.precedence) match {
      case Seq(single) if !isDocScope => single.resolveReference
      case _ => ReferenceResolver.lift { case LinkSource(ref: Reference, _) =>
        InvalidSpan(s"Ambiguous reference: more than one ${uniqueSelector.description} in path $path", ref.source)
      }
    }

    override def resolveReference(linkSource: LinkSource): Option[Span] = resolver(linkSource)

    private val indexed = targets.zipWithIndex.iterator
    def nextOption: Option[(TargetResolver, Int)] = if (indexed.hasNext) Some(indexed.next()) else None

    override def replaceTarget(rewrittenOriginal: Element): Option[Element] = nextOption.flatMap { case (target, index) =>
      target.replaceTarget(rewrittenOriginal).map { targetBase =>
        targetBase.options.id match {
          case Some(id) => targetBase.withId(id + "-" + (index + 1))
          case None => targetBase
        }
      }
    }
  }

  def forDelegate (selector: Selector, delegate: TargetResolver): TargetResolver = new TargetResolver(selector, delegate.targetFormats) {
    override def resolveReference (linkSource: LinkSource): Option[Span] = delegate.resolveReference(linkSource)
    override def replaceTarget (rewrittenOriginal: Element): Option[Element] = delegate.replaceTarget(rewrittenOriginal)
  }

}

/** Represents a resolver for a sequence of targets where matching reference nodes get determined by position.
  * The `resolveReference` and `resolveTarget` methods can be invoked as many times as this sequence contains elements.
  */
case class TargetSequenceResolver (targets: Seq[TargetResolver], sel: Selector) extends TargetResolver(sel) {
  private val refIt = targets.iterator
  private val targetIt = targets.iterator

  private def nextOption (it: Iterator[TargetResolver]) = if (it.hasNext) Some(it.next()) else None

  def resolveReference (linkSource: LinkSource): Option[Span] =
    nextOption(refIt).flatMap(_.resolveReference(linkSource))

  def replaceTarget (rewrittenOriginal: Element): Option[Element] =
    nextOption(targetIt).flatMap(_.replaceTarget(rewrittenOriginal))
}

case class LinkAliasResolver (sourceSelector: TargetIdSelector,
                              targetSelector: TargetIdSelector,
                              referenceResolver: LinkSource => Option[Span],
                              formats: TargetFormats) extends TargetResolver(sourceSelector, formats) {
  override def resolveReference (linkSource: LinkSource): Option[Span] = referenceResolver(linkSource)
  override def replaceTarget (rewrittenOriginal: Element): Option[Element] = None

  def resolveWith (referenceResolver: LinkSource => Option[Span]): LinkAliasResolver =
    copy(referenceResolver = referenceResolver)

  def circularReference: LinkAliasResolver = copy(referenceResolver =
    TargetResolver.forInvalidTarget(sourceSelector, s"circular link reference: ${targetSelector.id}").resolveReference)
}
object LinkAliasResolver {
  def unresolved (sourceSelector: TargetIdSelector, 
                  targetSelector: TargetIdSelector,
                  targetFormats: TargetFormats): LinkAliasResolver =
    apply(
      sourceSelector, 
      targetSelector,
      TargetResolver.forInvalidTarget(sourceSelector, s"unresolved link alias: ${targetSelector.id}").resolveReference,
      targetFormats
    )
}
