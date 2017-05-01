package laika.parse.core

/** A base class for parser results. A result is either successful or not
  *  (failure may be fatal, i.e., an Error, or not, i.e., a Failure). On
  *  success, provides a result of type `T` which consists of some result
  *  (and the rest of the input). */
sealed abstract class ParseResult[+T] {
  /** Functional composition of ParseResults.
    *
    * @param f the function to be lifted over this result
    * @return `f` applied to the result of this `ParseResult`, packaged up as a new `ParseResult`
    */
  def map[U](f: T => U): ParseResult[U]

  /** Partial functional composition of ParseResults.
    *
    * @param f the partial function to be lifted over this result
    * @param error a function that takes the same argument as `f` and
    *        produces an error message to explain why `f` wasn't applicable
    *        (it is called when this is the case)
    * @return if `f` f is defined at the result in this `ParseResult`, `f`
    *         applied to the result of this `ParseResult`, packaged up as
    *         a new `ParseResult`. If `f` is not defined, `Failure`.
    */
  def mapPartial[U](f: PartialFunction[T, U], error: T => String): ParseResult[U]

  def flatMapWithNext[U](f: T => ParserContext => ParseResult[U]): ParseResult[U]

  def filterWithError(p: T => Boolean, error: T => String, position: ParserContext): ParseResult[T]

  def append[U >: T](a: => ParseResult[U]): ParseResult[U]

  def isEmpty = !successful

  def get: T

  def getOrElse[B >: T](default: => B): B =
    if (isEmpty) default else this.get

  val next: ParserContext

  val successful: Boolean
}

/** The success case of `ParseResult`: contains the result and the remaining input.
  *
  *  @param result The parser's output
  *  @param next   The parser's remaining input
  */
case class Success[+T](result: T, next: ParserContext) extends ParseResult[T] {

  def map[U](f: T => U) = Success(f(result), next)

  def mapPartial[U](f: PartialFunction[T, U], error: T => String): ParseResult[U] =
    if (f.isDefinedAt(result)) Success(f(result), next)
    else Failure(new MessageFunction(result, error), next)

  def flatMapWithNext[U](f: T => ParserContext => ParseResult[U]): ParseResult[U] = f(result)(next)

  def filterWithError(p: T => Boolean, error: T => String, position: ParserContext): ParseResult[T] =
    if (p(result)) this
    else Failure(new MessageFunction(result, error), position)

  def append[U >: T](a: => ParseResult[U]): ParseResult[U] = this

  def get: T = result

  override def toString = s"[${next.position}] parsed: $result"

  val successful = true
}

/** The failure case of `ParseResult`: contains an error-message and the remaining input.
  *  Parsing will back-track when a failure occurs.
  *
  *  @param msgProvider  A provider that produces an error message for this failure based on its ParserContext
  *  @param next         The parser's unconsumed input at the point where the failure occurred.
  */
case class Failure(msgProvider: MessageProvider, next: ParserContext) extends ParseResult[Nothing] {

  lazy val message = msgProvider.message(next)

  val successful = false

  def map[U](f: Nothing => U) = this

  def mapPartial[U](f: PartialFunction[Nothing, U], error: Nothing => String): ParseResult[U] = this

  def flatMapWithNext[U](f: Nothing => ParserContext => ParseResult[U]): ParseResult[U] = this

  def filterWithError(p: Nothing => Boolean, error: Nothing => String, position: ParserContext): ParseResult[Nothing] = this

  def get: Nothing = scala.sys.error("No result when parsing failed")

  def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = { val alt = a; alt match {
    case Success(_, _) => alt
    case f: Failure => if (alt.next.offset < next.offset) this else alt
  }}

  override def toString = s"[${next.position}] failure: $message\n\n${next.position.lineContentWithCaret}"
}