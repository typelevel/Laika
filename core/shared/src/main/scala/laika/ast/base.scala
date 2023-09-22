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

import laika.parse.SourceFragment

/** The base class for all Elements forming the document tree.
  * Usually not extended directly, instead either `Span` or `Block` should be picked as the base type
  * for new element types.
  *
  * All node types have an optional id and zero or more associated styles serving as render hints.
  */
abstract class Element extends Product with Serializable {

  type Self <: Element

  def options: Options

  /** Indicates whether this element has the specified style assigned.
    */
  def hasStyle(name: String): Boolean = options.styles.contains(name)

  /** Indicates whether this element has an id assigned.
    */
  def hasId: Boolean = options.id.isDefined

  /** Returns a new instance of this element without its id.
    */
  def withoutId: Self = modifyOptions(opt => Options(None, opt.styles))

  /** Returns a new instance of this element with its id set to the specified value, overriding any existing value.
    */
  def withId(id: String): Self = modifyOptions(opt => Options(Some(id), opt.styles))

  /** Returns a new instance of this element with the specified style added to its existing styles.
    */
  def withStyle(name: String): Self = mergeOptions(Styles(name))

  /** Returns a new instance of this element with the specified styles added to its existing styles.
    */
  def withStyles(style: String, styles: String*): Self = mergeOptions(
    Styles((style +: styles).toSet)
  )

  /** Returns a new instance of this element with the specified styles added to its existing styles.
    */
  def withStyles(styles: Iterable[String]): Self = mergeOptions(Styles(styles.toSet))

  /** Returns a new instance of this element with its options merged with the specified options.
    */
  def mergeOptions(opt: Options): Self = modifyOptions(_ + opt)

  /** Returns a new instance of this element with the new options obtained from applying the specified function
    * to the existing value.
    */
  def modifyOptions(f: Options => Options): Self = withOptions(f(options))

  /** Returns a new instance of this element with all options removed from it.
    */
  def clearOptions: Self = withOptions(Options.empty)

  /** Returns a new instance of this element with the specified options replacing the current value.
    */
  def withOptions(options: Options): Self
}

/** Provides a fallback for elements the renderer does not know how to deal with.
  */
trait Fallback {

  /** Defines a fallback for this element in case the renderer does not know how to deal with it.
    */
  def fallback: Element
}

/** The base type for all block level elements.
  */
trait Block extends Element { type Self <: Block }

/** The base type for all inline elements.
  */
trait Span extends Element { type Self <: Span }

/** The base type for all list items.
  */
trait ListItem extends Element { type Self <: ListItem }

/** Represents a hidden element that will be ignored by renderers.
  *
  * These kind of nodes usually provide information that will be extracted
  * from the tree before AST transformations and renderers get applied.
  */
trait Hidden extends Element

/** Represents an element that needs to be resolved in an AST transformation step.
  *
  * Passing documents that still contain elements of this kind to a renderer
  * will usually be treated as errors.
  */
trait Unresolved extends Element {

  /** The fragment from the input source that produced this element.
    * Can be used to report the line of the error or to render a fallback that simply renders back
    * the consumed input.
    */
  def source: SourceFragment

  /** An error message to display when this element remains unresolved until after the final AST transformation step.
    */
  def unresolvedMessage: String
}

/** Represents an invalid element.
  * Renderers can choose to either render the fallback or the runtime message or both,
  * depending on the configuration of the transformer or renderer.
  */
trait Invalid extends Element with Fallback {

  type FallbackElement <: Element

  /** The fragment from the input source that produced this element.
    * Can be used to report the line of the error or to render a fallback that simply renders back
    * the consumed input.
    */
  def source: SourceFragment

  /** A message describing the reason why this element is invalid.
    */
  def message: RuntimeMessage

  /** A fallback that can be used in case a transformer or renderer is configured in such a way that
    * errors are ignored.
    * Renderers will pick this fallback element instead of the original invalid element in such a case.
    */
  def fallback: FallbackElement
}

/** The base type for all reference elements.
  *
  * A reference points to some other node in the document tree and needs
  * to be resolved and replaced by a rewrite rule before rendering.
  * Therefore none of the available renderers include logic for dealing with references.
  */
trait Reference extends Span with Unresolved {
  type Self <: Reference
}

/** Represents a definition that can be used to resolve references.
  *
  *  Only part of the raw document tree and then removed or replaced
  *  by a rewrite rule before rendering.
  */
trait Definition extends Block { type Self <: Definition }

/** The base type for all link elements.
  *
  * In contrast to the reference type, it is only mixed in by elements representing resolved links
  * that can be dealt with by renderers.
  */
sealed trait Link extends Span { type Self <: Link }

/** A local link that always points to a target within the same document.
  */
trait LocalLink extends Link {
  type Self <: LocalLink

  /** The id of the node within the same document that this link points to. */
  def refId: String
}

/** A global link that can point to any document within the input tree or to an external target.
  */
trait GlobalLink extends Link {
  type Self <: GlobalLink

  /** The target this link points to. */
  def target: Target

  /** Creates a new instance of this node pointing to the specified target instead.
    */
  def withTarget(target: Target): Self

  /** Indicates whether this node type supports external targets for *all* output formats.
    * This is not true for images, for example, as they require embedding for EPUB and PDF formats.
    */
  def supportsExternalTargets: Boolean
}

/** The base type for all link targets. The id has to be
  *  unique for the whole document across all types
  *  of `LinkTarget` implementations.
  */
trait LinkTarget extends Block { type Self <: LinkTarget }
