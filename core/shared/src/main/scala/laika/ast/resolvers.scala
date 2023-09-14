package laika.ast

import laika.config.{ ConfigError, ConfigValue, Key }
import ConfigError.InvalidType
import laika.config.ConfigValue.{ ASTValue, SimpleValue }

/** Represents a placeholder inline element that needs
  *  to be resolved in a rewrite step.
  *  Useful for elements that need access to the
  *  document, structure, title or configuration before
  *  being fully resolved.
  */
trait SpanResolver extends Span with Unresolved {
  def resolve(cursor: DocumentCursor): Span
  def runsIn(phase: RewritePhase): Boolean
}

import laika.parse.SourceFragment

/** Represents a placeholder block element that needs to be resolved in a rewrite step.
  *  Useful for elements that need access to the document, structure, title
  *  or configuration before being fully resolved.
  */
trait BlockResolver extends Block with Unresolved {
  def resolve(cursor: DocumentCursor): Block
  def runsIn(phase: RewritePhase): Boolean
}

/** Represents an element that introduces new context that can be used in substitution references
  * in any child element.
  *
  * Usually used in directive implementations and not contributing to the rendered output itself.
  */
trait ElementScope[E <: Element] extends Unresolved {
  def content: E
  def context: ConfigValue
}

/** Represents a block element that introduces new context that can be used in substitution references
  * in any child element.
  *
  * Usually used in directive implementations and not contributing to the rendered output itself.
  */
case class BlockScope(
    content: Block,
    context: ConfigValue,
    source: SourceFragment,
    options: Options = NoOpt
) extends ElementScope[Block] with Block {
  type Self = BlockScope
  def withOptions(options: Options): BlockScope = copy(options = options)
  lazy val unresolvedMessage: String            = s"Unresolved block scope"
}

/** Represents a span element that introduces new context that can be used in substitution references
  * in any child element.
  *
  * Usually used in directive implementations and not contributing to the rendered output itself.
  */
case class SpanScope(
    content: Span,
    context: ConfigValue,
    source: SourceFragment,
    options: Options = NoOpt
) extends ElementScope[Span] with Span {
  type Self = SpanScope
  def withOptions(options: Options): SpanScope = copy(options = options)
  lazy val unresolvedMessage: String           = s"Unresolved span scope"
}

/** Represents a template span element that introduces new context that can be used in substitution references
  * in any child element.
  *
  * Usually used in directive implementations and not contributing to the rendered output itself.
  */
case class TemplateScope(
    content: TemplateSpan,
    context: ConfigValue,
    source: SourceFragment,
    options: Options = NoOpt
) extends ElementScope[TemplateSpan] with TemplateSpan {
  type Self = TemplateScope
  def withOptions(options: Options): TemplateScope = copy(options = options)
  lazy val unresolvedMessage: String               = s"Unresolved template scope"
}

/** Represents a reference to a value from the context
  *  of the current document. The `ref` attribute
  *  is a simple path expression in dot notation
  *  for accessing properties of an object (or keys
  *  of a Map).
  *
  *  The root elements accessible to such a reference are:
  *
  *  - `document`: the current document with all of its public properties
  *  - `parent`: the parent tree of the current document
  *  - `root`: the root tree
  *  - `config`: all configuration values for the current document,
  *    including those inherited from parent trees
  */
abstract class ContextReference[T <: Span](ref: Key, source: SourceFragment) extends SpanResolver {

  protected def missing: InvalidSpan = InvalidSpan(s"Missing required reference: '$ref'", source)

  protected def invalid(cError: ConfigError): InvalidSpan =
    InvalidSpan(s"Error resolving reference: '$ref': ${cError.message}", source)

  protected def invalidType(value: ConfigValue): InvalidSpan = InvalidSpan(
    s"Error resolving reference: '$ref': " +
      InvalidType("AST Element or Simple Value", value).message,
    source
  )

}

/** A context reference specifically for use in template documents.
  */
case class TemplateContextReference(
    ref: Key,
    required: Boolean,
    source: SourceFragment,
    options: Options = NoOpt
) extends ContextReference[TemplateSpan](ref, source) with TemplateSpan {
  type Self = TemplateContextReference

  def resolve(cursor: DocumentCursor): Span = cursor.resolveReference(ref) match {
    case Right(Some(ASTValue(s: TemplateSpan)))         => s
    case Right(Some(ASTValue(RootElement(content, _)))) => EmbeddedRoot(content)
    case Right(Some(ASTValue(e: Element)))              => TemplateElement(e)
    case Right(Some(simple: SimpleValue))         => TemplateString(simple.render)
    case Right(None) if !required                       => TemplateString("")
    case Right(None)                                    => TemplateElement(missing)
    case Right(Some(unsupported))                       => TemplateElement(invalidType(unsupported))
    case Left(configError)                              => TemplateElement(invalid(configError))
  }

  def withOptions(options: Options): TemplateContextReference = copy(options = options)
  def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

  lazy val unresolvedMessage: String =
    s"Unresolved template context reference with key '${ref.toString}'"

}

/** A context reference specifically for use in markup documents.
  */
case class MarkupContextReference(
    ref: Key,
    required: Boolean,
    source: SourceFragment,
    options: Options = NoOpt
) extends ContextReference[Span](ref, source) {
  type Self = MarkupContextReference

  def resolve(cursor: DocumentCursor): Span = cursor.resolveReference(ref) match {
    case Right(Some(ASTValue(s: Span)))         => s
    case Right(Some(ASTValue(e: Element)))      => TemplateElement(e)
    case Right(Some(simple: SimpleValue)) => Text(simple.render)
    case Right(None) if !required               => Text("")
    case Right(None)                            => missing
    case Right(Some(unsupported))               => invalidType(unsupported)
    case Left(configError)                      => invalid(configError)
  }

  def withOptions(options: Options): MarkupContextReference = copy(options = options)

  def runsIn(phase: RewritePhase): Boolean =
    phase.isInstanceOf[RewritePhase.Render] // TODO - test earlier phases

  lazy val unresolvedMessage: String =
    s"Unresolved markup context reference with key '${ref.toString}'"

}
