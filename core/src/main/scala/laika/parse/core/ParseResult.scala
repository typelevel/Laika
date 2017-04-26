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

  def flatMapWithNext[U](f: T => Reader => ParseResult[U]): ParseResult[U]

  def filterWithError(p: T => Boolean, error: T => String, position: Reader): ParseResult[T]

  def append[U >: T](a: => ParseResult[U]): ParseResult[U]

  def isEmpty = !successful

  /** Returns the embedded result. */
  def get: T

  def getOrElse[B >: T](default: => B): B =
    if (isEmpty) default else this.get

  val next: Reader

  val successful: Boolean
}

/** The success case of `ParseResult`: contains the result and the remaining input.
  *
  *  @param result The parser's output
  *  @param next   The parser's remaining input
  */
case class Success[+T](result: T, override val next: Reader) extends ParseResult[T] {
  def map[U](f: T => U) = Success(f(result), next)
  def mapPartial[U](f: PartialFunction[T, U], error: T => String): ParseResult[U]
  = if(f.isDefinedAt(result)) Success(f(result), next)
  else Failure(error(result), next)

  def flatMapWithNext[U](f: T => Reader => ParseResult[U]): ParseResult[U]
  = f(result)(next)

  def filterWithError(p: T => Boolean, error: T => String, position: Reader): ParseResult[T] =
    if (p(result)) this
    else Failure(error(result), position)

  def append[U >: T](a: => ParseResult[U]): ParseResult[U] = this

  def get: T = result

  /** The toString method of a Success. */
  override def toString = "["+next.pos+"] parsed: "+result

  val successful = true
}

/** The failure case of `ParseResult`: contains an error-message and the remaining input.
  *  Parsing will back-track when a failure occurs.
  *
  *  @param msg    An error message string describing the failure.
  *  @param next   The parser's unconsumed input at the point where the failure occurred.
  */
case class Failure(msg: String, next: Reader) extends ParseResult[Nothing] {

  val successful = false

  def map[U](f: Nothing => U) = this
  def mapPartial[U](f: PartialFunction[Nothing, U], error: Nothing => String): ParseResult[U] = this

  def flatMapWithNext[U](f: Nothing => Reader => ParseResult[U]): ParseResult[U] = this

  def filterWithError(p: Nothing => Boolean, error: Nothing => String, position: Reader): ParseResult[Nothing] = this

  def get: Nothing = scala.sys.error("No result when parsing failed")

  def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = { val alt = a; alt match {
    case Success(_, _) => alt
    case f: Failure => if (alt.next.pos < next.pos) this else alt
  }}

  /** The toString method of a Failure yields an error message. */
  override def toString = "["+next.pos+"] failure: "+msg+"\n\n"+next.pos.longString
}