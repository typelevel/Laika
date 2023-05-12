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

package laika.ast

import cats.syntax.all._
import laika.ast.RewriteRules.{ ChainedRewriteRules, RewriteRulesBuilder }
import laika.config.Config.ConfigResult
import laika.config.ConfigErrors
import laika.factory.{ RenderFormat, TwoPhaseRenderFormat }
import laika.rewrite.{ OutputContext, TemplateFormatter, UnresolvedNodeDetector }
import laika.rewrite.link.LinkResolver
import laika.rewrite.nav.{ SectionBuilder, Selections }

import scala.annotation.tailrec

/** A set of rewrite rules describing a set of modifications to be applied to an AST of a document.
  *
  * For reasons of type-safety the major element type blocks and spans (in markup documents) and template spans
  * (in template documents) have their separate set of rules, as an element in a block position for example
  * can usually only be replaced by another block and not by any other element type.
  *
  * @author Jens Halm
  */
case class RewriteRules(
    spanRules: Seq[RewriteRule[Span]] = Nil,
    blockRules: Seq[RewriteRule[Block]] = Nil,
    templateRules: Seq[RewriteRule[TemplateSpan]] = Nil
) {

  private lazy val chainedSpanRules: Span => RewriteAction[Span] = ChainedRewriteRules(spanRules)

  private lazy val chainedBlockRules: Block => RewriteAction[Block] = ChainedRewriteRules(
    blockRules
  )

  private lazy val chainedTemplateRules: TemplateSpan => RewriteAction[TemplateSpan] =
    ChainedRewriteRules(templateRules)

  /** Combines the rules defined in this instance with the rules defined
    * in the specified other instance. If a rule in this instance matches the same
    * instance as another rule in the other instance, the rule in this instance
    * will be applied first, before its result gets passed to the other function.
    */
  def ++ (other: RewriteRules): RewriteRules =
    RewriteRules(
      spanRules ++ other.spanRules,
      blockRules ++ other.blockRules,
      templateRules ++ other.templateRules
    )

  /** Rewrites the specified element based on the set of rules in this instance.
    *
    * If the rule is not defined for the element or the rule returns
    * a `Retain` action as a result the old element will be returned unchanged.
    *
    * If it returns `Replace` with a new element that element will be returned.
    * If it returns `Remove` then an empty element container will be returned.
    *
    * If the specified element type does not support rewriting, it is returned unchanged.
    *
    * The rewriting is performed bottom-up (depth-first), including any children of the
    * specified element, therefore an element passed to the rule only contains
    * children which have already been processed.
    */
  def rewriteElement(element: Element): Element = element match {
    case b: Block               => rewriteBlock(b)
    case t: TemplateSpan        => rewriteTemplateSpan(t)
    case s: Span                => rewriteSpan(s)
    case r: RewritableContainer => r.rewriteChildren(this)
    case other                  => other
  }

  /** Rewrites the specified span based on the set of rules in this instance.
    *
    * If the rule is not defined for the span or the rule returns
    * a `Retain` action as a result the old span will be returned unchanged.
    *
    * If it returns `Replace` with a new span that span will be returned.
    * If it returns `Remove` then an empty span container will be returned.
    *
    * The rewriting is performed bottom-up (depth-first), including any children of the
    * specified span, therefore an element passed to the rule only contains
    * children which have already been processed.
    */
  def rewriteSpan(span: Span): Span =
    rewriteSpans(Seq(span)).headOption.getOrElse(SpanSequence.empty)

  /** Rewrites the specified block based on the set of rules in this instance.
    *
    * If the rule is not defined for the block or the rule returns
    * a `Retain` action as a result the old block will be returned unchanged.
    *
    * If it returns `Replace` with a new block that block will be returned.
    * If it returns `Remove` then an empty block container will be returned.
    *
    * The rewriting is performed bottom-up (depth-first), including any children (blocks and spans)
    * of the specified block, therefore an element passed to the rule only contains
    * children which have already been processed.
    */
  def rewriteBlock(block: Block): Block =
    rewriteBlocks(Seq(block)).headOption.getOrElse(BlockSequence.empty)

  /** Rewrites the specified template span based on the set of rules in this instance.
    *
    * If the rule is not defined for the span or the rule returns
    * a `Retain` action as a result the old span will be returned unchanged.
    *
    * If it returns `Replace` with a new span that span will be returned.
    * If it returns `Remove` then an empty template span container will be returned.
    *
    * The rewriting is performed bottom-up (depth-first), including any children of the
    * specified template span, therefore an element passed to the rule only contains
    * children which have already been processed.
    */
  def rewriteTemplateSpan(span: TemplateSpan): TemplateSpan =
    rewriteTemplateSpans(Seq(span)).headOption.getOrElse(TemplateSpanSequence.empty)

  /** Rewrites the specified sequence of spans based on the set of rules in this instance.
    *
    * If the rule is not defined for a specific span or the rule returns
    * a `Retain` action as a result the old span remains in the tree unchanged.
    *
    * If it returns `Remove` then the span gets removed from the ast,
    * if it returns `Replace` with a new span it will replace the old one.
    *
    * The size of the returned `Seq` is always between 0 and the number of spans passed to
    * this function.
    *
    * The rewriting is performed bottom-up (depth-first), therefore
    * any element passed to the rule only contains children which have already
    * been processed.
    */
  def rewriteSpans(spans: Seq[Span]): Seq[Span] = rewrite(chainedSpanRules, spans)

  /** Rewrites the specified sequence of blocks based on the set of rules in this instance.
    *
    * If the rule is not defined for a specific block or the rule returns
    * a `Retain` action as a result the old block remains in the tree unchanged.
    *
    * If it returns `Remove` then the block gets removed from the ast,
    * if it returns `Replace` with a new block it will replace the old one.
    *
    * The size of the returned `Seq` is always between 0 and the number of blocks passed to
    * this function.
    *
    * The rewriting is performed bottom-up (depth-first), therefore
    * any element passed to the rule only contains children which have already
    * been processed. If the blocks contain span elements, the span rules defined
    * in this instance will also be applied recursively.
    */
  def rewriteBlocks(blocks: Seq[Block]): Seq[Block] = rewrite(chainedBlockRules, blocks)

  /** Rewrites the specified sequence of template spans based on the set of rules in this instance.
    *
    * If the rule is not defined for a specific span or the rule returns
    * a `Retain` action as a result the old span remains in the tree unchanged.
    *
    * If it returns `Remove` then the span gets removed from the ast,
    * if it returns `Replace` with a new span it will replace the old one.
    *
    * The size of the returned `Seq` is always between 0 and the number of template spans passed to
    * this function.
    *
    * The rewriting is performed bottom-up (depth-first), therefore
    * any element passed to the rule only contains children which have already
    * been processed.
    */
  def rewriteTemplateSpans(spans: Seq[TemplateSpan]): Seq[TemplateSpan] =
    rewrite(chainedTemplateRules, spans)

  private def rewrite[T <: AnyRef](childRules: T => RewriteAction[T], children: Seq[T]): Seq[T] = {

    def newChild(oldChild: T, action: RewriteAction[T]): Option[T] = action match {
      case Retain     => Some(oldChild)
      case Remove     => None
      case Replace(e) => Some(e)
    }

    children flatMap {
      case rw: RewritableContainer =>
        val rewritten = rw.rewriteChildren(this).asInstanceOf[T]
        val action    = childRules(rewritten)
        newChild(rewritten, action)
      case child                   =>
        newChild(child, childRules(child))
    }
  }

  def asBuilder: RewriteRulesBuilder = _ => Right(this)

}

/** Factory methods and utilities for dealing with rewrite rules.
  *
  * @author Jens Halm
  */
object RewriteRules {

  type RewriteRulesBuilder = DocumentCursor => ConfigResult[RewriteRules]

  type RewritePhaseBuilder = PartialFunction[RewritePhase, Seq[RewriteRulesBuilder]]

  /** Creates a new instance without any rules. Applying an empty instance to an AST will always
    * return the AST unchanged.
    */
  def empty: RewriteRules = RewriteRules()

  /** Creates a new instance containing only this single rule for spans.
    */
  def forSpans(rule: RewriteRule[Span]): RewriteRules = RewriteRules(spanRules = Seq(rule))

  /** Creates a new instance containing only this single rule for blocks.
    */
  def forBlocks(rule: RewriteRule[Block]): RewriteRules = RewriteRules(blockRules = Seq(rule))

  /** Creates a new instance containing only this single rule for template spans.
    */
  def forTemplates(rule: RewriteRule[TemplateSpan]): RewriteRules =
    RewriteRules(templateRules = Seq(rule))

  /** Chains the specified rewrite rules so that they get applied to matching elements
    * in the order specified in the given sequence.
    */
  case class ChainedRewriteRules[T](rules: Seq[RewriteRule[T]]) extends (T => RewriteAction[T]) {

    def apply(element: T): RewriteAction[T] = {

      @tailrec
      def applyNextRule(
          currentAction: RewriteAction[T],
          remainingRules: Seq[RewriteRule[T]]
      ): RewriteAction[T] =
        if (currentAction == Remove || remainingRules.isEmpty) currentAction
        else {
          val input      = currentAction match {
            case Replace(elem) => elem
            case _             => element
          }
          val nextAction =
            remainingRules.head.applyOrElse[T, RewriteAction[T]](input, _ => currentAction) match {
              case Retain => currentAction
              case other  => other
            }
          applyNextRule(nextAction, remainingRules.tail)
        }

      applyNextRule(Retain, rules)
    }

  }

  /** Chains the specified rule factory functions into a single factory function.
    */
  def chainFactories(rules: Seq[RewriteRulesBuilder]): RewriteRulesBuilder =
    cursor =>
      rules.toList
        .map(_(cursor).toEitherNec)
        .parSequence
        .map(_.reduceOption(_ ++ _).getOrElse(RewriteRules.empty))
        .leftMap(ConfigErrors.apply)

  /** The default built-in rewrite rules, dealing with section building and link resolution.
    * These are not installed as part of any default extension bundle as they have specific
    * ordering requirements not compatible with the standard bundle ordering in `OperationConfig`.
    */
  def defaultsFor(
      root: DocumentTreeRoot,
      phase: RewritePhase,
      slugBuilder: String => String
  ): Seq[RewriteRulesBuilder] = phase match {
    case RewritePhase.Build     => Nil
    case RewritePhase.Resolve   => Seq(new LinkResolver(root, slugBuilder), SectionBuilder)
    case RewritePhase.Render(_) =>
      Seq(
        Selections.FormatFilter,
        NavigationList.FormatFilter,
        TemplateFormatter,
        UnresolvedNodeDetector
      )
  }

}

/** Describes the action to be performed for a particular node in the document AST.
  */
sealed trait RewriteAction[+T]

/** Indicates that the element a rewrite rule had been applied to should be replaced with this new value.
  */
case class Replace[T](newValue: T) extends RewriteAction[T]

/** Indicates that the element a rewrite rule had been applied to should be kept in the document AST unchanged.
  */
case object Retain extends RewriteAction[Nothing]

/** Indicates that the element a rewrite rule had been applied to should be removed from the document AST.
  */
case object Remove extends RewriteAction[Nothing]

/** Represents one of the rewrite phases for document AST transformations.
  *
  * These transformations are performed between parsing and rendering and deal with tasks like
  * link validation, resolving substitution variables, directive processing or other tasks.
  *
  * A phased model allows to separate rules that contribute new nodes to the AST from nodes
  * that analyze the existing AST, e.g. for producing navigation artifacts.
  * Running them all in one phase would create a chicken-and-egg scenario that would usually
  * lead to undesired or unexpected results.
  */
sealed trait RewritePhase

case object RewritePhase {

  /** Represents the first rewrite phase after parsing.
    *
    * This is the only phase where the introduction of new link targets is still allowed.
    * By default all directives and all rewrite rules that do *not* have access to a document cursor
    * run in this phase.
    */
  case object Build extends RewritePhase

  /** Represents the second rewrite phase between parsing and rendering.
    *
    * By default no user rules or directives run in this phase,
    * it is mostly reserved for the internal rules for link resolvers and similar tasks.
    */
  case object Resolve extends RewritePhase

  /** Represents the final rewrite phase before rendering.
    *
    * This phase is specific to the output format and therefore the only phase type that is parameterized.
    * By default all directives and all rewrite rules that do have access to a document cursor
    * run in this phase to ensure that their cursor represents a state that is close to the final AST
    * passed to the renderer.
    */
  case class Render(context: OutputContext) extends RewritePhase

  object Render {
    def apply(format: RenderFormat[_]): Render            = apply(OutputContext(format))
    def apply(format: TwoPhaseRenderFormat[_, _]): Render = apply(OutputContext(format))
  }

}
