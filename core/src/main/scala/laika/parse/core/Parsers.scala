/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package laika.parse.core

import laika.parse.core.text.Literal

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/** `Parsers` is a component that ''provides'' generic parser combinators.
 *
 *  There are two aspects to the result of a parser:
 *  1. success or failure
 *  1. the result.
 *
 *  The term ''parser combinator'' refers to the fact that these parsers
 *  are constructed from primitive parsers and composition operators, such
 *  as sequencing, alternation, optionality, repetition, lifting, and so on. For example,
 *  given `p1` and `p2` of type [[laika.parse.core.Parser]]:
 *
 *  {{{
 *  p1 ~ p2 // sequencing: must match p1 followed by p2
 *  p1 | p2 // alternation: must match either p1 or p2, with preference given to p1
 *  p1.?    // optionality: may match p1 or not
 *  p1.*    // repetition: matches any number of repetitions of p1
 *  }}}
 *
 *  These combinators are provided as methods on [[laika.parse.core.Parser]],
 *  or as methods taking one or more `Parsers` and returning a `Parser` provided in
 *  this class.
 *
 *  A ''primitive parser'' is a parser that accepts or rejects a single
 *  piece of input, based on a certain criterion, such as whether the
 *  input...
 *  - is equal to some given object (see method `accept`),
 *  - satisfies a certain predicate (see method `acceptIf`),
 *  - is in the domain of a given partial function (see method `acceptMatch`)
 *  - or other conditions, by using one of the other methods available, or subclassing `Parser`
 *
 *  Even more primitive parsers always produce the same result, irrespective of the input. See
 *  methods `success` and `failure` as examples.
 *
 *  @author Martin Odersky
 *  @author Iulian Dragos
 *  @author Adriaan Moors
 */
trait Parsers {


  def Parser[T](f: Reader => ParseResult[T]): Parser[T]
    = new Parser[T]{ def apply(in: Reader) = f(in) }


  /** A parser that matches only the given element `e`.
    *
    *  `elem(e)` succeeds if the input starts with an element `e`.
    *
    *  The method is implicit so that elements can automatically be lifted to their parsers.
    *  For example, when parsing `Token`s, `Identifier("new")` (which is a `Token`) can be used directly,
    *  instead of first creating a `Parser` using `elem(Identifier("new"))`.
    *
    *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
    *  @return a `Parser` that succeeds if `e` is the next available input (and returns it).
    */
  implicit def char(e: Char): Parser[Char] = acceptIf(_ == e)("'"+e+"' expected but " + _ + " found")

  /** A parser matching input elements that satisfy a given predicate.
   *
   *  `acceptIf(p)(el => "Unexpected "+el)` succeeds if the input starts with an element `e` for which `p(e)` is true.
   *
   *  @param  err    A function from the received element into an error message.
   *  @param  p      A predicate that determines which elements match.
   *  @return        A parser for elements satisfying p(e).
   */
  def acceptIf(p: Char => Boolean)(err: Char => String): Parser[Char] = Parser { in =>
    if (in.atEnd) Failure(Message.UnexpectedEOF, in)
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure(new MessageFunction(in.first, err), in) // TODO - avoid object creation
  }

  /** A parser that matches a literal string */
  implicit def literal (expected: String): Parser[String] = Literal(expected)

  /** A parser that always fails.
   *
   * @param msg The error message describing the failure.
   * @return A parser that always fails with the specified error message.
   */
  def failure(msg: String) = Parser{ in => Failure(Message(msg), in) }

  /** A parser that always succeeds.
   *
   * @param v The result for the parser
   * @return A parser that always succeeds, with the given result `v`
   */
  def success[T](v: T) = Parser{ in => Success(v, in) }

  /** A parser generator for repetitions.
   *
   *  `rep(p)` repeatedly uses `p` to parse the input until `p` fails
   *  (the result is a List of the consecutive results of `p`).
   *
   * @param p a `Parser` that is to be applied successively to the input
   * @return A parser that returns a list of results produced by repeatedly applying `p` to the input.
   */
  def rep[T](p: => Parser[T]): Parser[List[T]] = rep1(p) | success(List())

  /** A parser generator for interleaved repetitions.
   *
   *  `repsep(p, q)` repeatedly uses `p` interleaved with `q` to parse the input, until `p` fails.
   *  (The result is a `List` of the results of `p`.)
   *
   *  Example: `repsep(term, ",")` parses a comma-separated list of term's, yielding a list of these terms.
   *
   * @param p a `Parser` that is to be applied successively to the input
   * @param q a `Parser` that parses the elements that separate the elements parsed by `p`
   * @return A parser that returns a list of results produced by repeatedly applying `p` (interleaved with `q`) to the input.
   *         The results of `p` are collected in a list. The results of `q` are discarded.
   */
  def repsep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    rep1sep(p, q) | success(List())

  /** A parser generator for non-empty repetitions.
   *
   *  `rep1(p)` repeatedly uses `p` to parse the input until `p` fails -- `p` must succeed at least
   *             once (the result is a `List` of the consecutive results of `p`)
   *
   * @param p a `Parser` that is to be applied successively to the input
   * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
   *        (and that only succeeds if `p` matches at least once).
   */
  def rep1[T](p: => Parser[T]): Parser[List[T]] = rep1(p, p)

  /** A parser generator for non-empty repetitions.
   *
   *  `rep1(f, p)` first uses `f` (which must succeed) and then repeatedly
   *     uses `p` to parse the input until `p` fails
   *     (the result is a `List` of the consecutive results of `f` and `p`)
   *
   * @param first a `Parser` that parses the first piece of input
   * @param p0 a `Parser` that is to be applied successively to the rest of the input (if any) -- evaluated at most once, and only when necessary
   * @return A parser that returns a list of results produced by first applying `f` and then
   *         repeatedly `p` to the input (it only succeeds if `f` matches).
   */
  def rep1[T](first: => Parser[T], p0: => Parser[T]): Parser[List[T]] = Parser { in =>
    lazy val p = p0 // lazy argument
    val elems = new ListBuffer[T]

    def continue(in: Reader): ParseResult[List[T]] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Reader): ParseResult[List[T]] = p0(in0) match {
        case Success(x, rest) => elems += x ; applyp(rest)
        case _                => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first(in) match {
      case Success(x, rest) => elems += x ; continue(rest)
      case f: Failure       => f
    }
  }

  /** A parser generator for a specified number of repetitions.
   *
   *  `repN(n, p)` uses `p` exactly `n` time to parse the input
   *  (the result is a `List` of the `n` consecutive results of `p`).
   *
   * @param p   a `Parser` that is to be applied successively to the input
   * @param num the exact number of times `p` must succeed
   * @return    A parser that returns a list of results produced by repeatedly applying `p` to the input
   *        (and that only succeeds if `p` matches exactly `n` times).
   */
  def repN[T](num: Int, p: => Parser[T]): Parser[List[T]] =
    if (num == 0) success(Nil) else Parser { in =>
      val elems = new ListBuffer[T]
      val p0 = p    // avoid repeatedly re-evaluating by-name parser

      @tailrec def applyp(in0: Reader): ParseResult[List[T]] =
        if (elems.length == num) Success(elems.toList, in0)
        else p0(in0) match {
          case Success(x, rest) => elems += x ; applyp(rest)
          case f: Failure       => f
        }

      applyp(in)
    }

  /** A parser generator for non-empty repetitions.
   *
   *  `rep1sep(p, q)` repeatedly applies `p` interleaved with `q` to parse the
   *  input, until `p` fails. The parser `p` must succeed at least once.
   *
   * @param p a `Parser` that is to be applied successively to the input
   * @param q a `Parser` that parses the elements that separate the elements parsed by `p`
   *          (interleaved with `q`)
   * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
   *         (and that only succeeds if `p` matches at least once).
   *         The results of `p` are collected in a list. The results of `q` are discarded.
   */
  def rep1sep[T](p : => Parser[T], q : => Parser[Any]): Parser[List[T]] =
    p ~ rep(q ~> p) ^^ {case x~y => x::y}

  /** A parser generator for optional sub-phrases.
   *
   *  `opt(p)` is a parser that returns `Some(x)` if `p` returns `x` and `None` if `p` fails.
   *
   * @param p A `Parser` that is tried on the input
   * @return a `Parser` that always succeeds: either with the result provided by `p` or
   *         with the empty result
   */
  def opt[T](p: => Parser[T]): Parser[Option[T]] =
    p ^^ (x => Some(x)) | success(None)

  /** Wrap a parser so that its failures and errors become success and
   *  vice versa -- it never consumes any input.
   */
  def not[T](p: => Parser[T]): Parser[Unit] = Parser { in =>
    p(in) match {
      case Success(_, _)  => Failure(Message.ExpectedFailure, in)
      case _              => Success((), in)
    }
  }

  /** A parser generator for guard expressions. The resulting parser will
   *  fail or succeed just like the one given as parameter but it will not
   *  consume any input.
   *
   * @param p a `Parser` that is to be applied to the input
   * @return A parser that returns success if and only if `p` succeeds but
   *         never consumes any input
   */
  def guard[T](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match{
      case s@ Success(s1,_) => Success(s1, in)
      case e => e
    }
  }

  /** A parser generator delimiting whole phrases (i.e. programs).
   *
   *  `consumeAll(p)` succeeds if `p` succeeds and no input is left over after `p`.
   *
   *  @param p the parser that must consume all input for the resulting parser
   *           to succeed.
   *  @return  a parser that has the same result as `p`, but that only succeeds
   *           if `p` consumed all the input.
   */
  def consumeAll[T](p: Parser[T]) = Parser[T] { in =>
      p(in) match {
        case s @ Success(out, in1) =>
          if (in1.atEnd) s
          else Failure(Message.ExpectedEOF, in1)
        case ns => ns
      }
    }

  /** Given a concatenation with a repetition (list), move the concatenated element into the list */
  def mkList[T] = (_: ~[T, List[T]]) match { case x ~ xs => x :: xs }

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: Reader): ParseResult[T] = p(in)

  /** Parse some prefix of character sequence `in` with parser `p`. */
  def parse[T](p: Parser[T], in: String): ParseResult[T] = p(new CharSequenceReader(in))

  /** Parse all of reader `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: Reader): ParseResult[T] = parse(consumeAll(p), in)

  /** Parse all of character sequence `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = parse(consumeAll(p), in)

}

object Parsers extends Parsers

/** A wrapper over sequence of matches.
  *
  *  Given `p1: Parser[A]` and `p2: Parser[B]`, a parser composed with
  *  `p1 ~ p2` will have type `Parser[~[A, B]]`. The successful result
  *  of the parser can be extracted from this case class.
  *
  *  It also enables pattern matching, so something like this is possible:
  *
  *  {{{
  *  def concat(p1: Parser[String], p2: Parser[String]): Parser[String] =
  *    p1 ~ p2 ^^ { case a ~ b => a + b }
  *  }}}
  */
case class ~[+a, +b](_1: a, _2: b) {
  override def toString = "("+ _1 +"~"+ _2 +")"
}