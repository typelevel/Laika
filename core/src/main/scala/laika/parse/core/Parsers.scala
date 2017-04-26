/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package laika.parse.core

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.DynamicVariable

// TODO: better error handling (labelling like parsec's <?>)

/** `Parsers` is a component that ''provides'' generic parser combinators.
 *
 *  There are two abstract members that must be defined in order to
 *  produce parsers: the type `Elem` and
 *  [[scala.util.parsing.combinator.Parsers.Parser]]. There are helper
 *  methods that produce concrete `Parser` implementations -- see ''primitive
 *  parser'' below.
 *
 *  A `Parsers` may define multiple `Parser` instances, which are combined
 *  to produced the desired parser.
 *
 *  The type of the elements these parsers should parse must be defined
 *  by declaring `Elem`
 *  (each parser is polymorphic in the type of result it produces).
 *
 *  There are two aspects to the result of a parser:
 *  1. success or failure
 *  1. the result.
 *
 *  A [[scala.util.parsing.combinator.Parsers.Parser]] produces both kinds of information,
 *  by returning a [[scala.util.parsing.combinator.Parsers.ParseResult]] when its `apply`
 *  method is called on an input.
 *
 *  The term ''parser combinator'' refers to the fact that these parsers
 *  are constructed from primitive parsers and composition operators, such
 *  as sequencing, alternation, optionality, repetition, lifting, and so on. For example,
 *  given `p1` and `p2` of type [[scala.util.parsing.combinator.Parsers.Parser]]:
 *
 *  {{{
 *  p1 ~ p2 // sequencing: must match p1 followed by p2
 *  p1 | p2 // alternation: must match either p1 or p2, with preference given to p1
 *  p1.?    // optionality: may match p1 or not
 *  p1.*    // repetition: matches any number of repetitions of p1
 *  }}}
 *
 *  These combinators are provided as methods on [[scala.util.parsing.combinator.Parsers.Parser]],
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
 *  methods `success`, `err` and `failure` as examples.
 *
 *  @see [[scala.util.parsing.combinator.RegexParsers]] and other known subclasses for practical examples.
 *
 *  @author Martin Odersky
 *  @author Iulian Dragos
 *  @author Adriaan Moors
 */
trait Parsers {
  /** the type of input elements the provided parsers consume (When consuming
   *  invidual characters, a parser is typically called a ''scanner'', which
   *  produces ''tokens'' that are consumed by what is normally called a ''parser''.
   *  Nonetheless, the same principles apply, regardless of the input type.) */
  type Elem

  /** The parser input is an abstract reader of input elements, i.e. the type
   *  of input the parsers in this component expect. */
  type Input = Reader[Elem]

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

    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U]

    def filterWithError(p: T => Boolean, error: T => String, position: Input): ParseResult[T]

    def append[U >: T](a: => ParseResult[U]): ParseResult[U]

    def isEmpty = !successful

    /** Returns the embedded result. */
    def get: T

    def getOrElse[B >: T](default: => B): B =
        if (isEmpty) default else this.get

    val next: Input

    val successful: Boolean
  }

  /** The success case of `ParseResult`: contains the result and the remaining input.
   *
   *  @param result The parser's output
   *  @param next   The parser's remaining input
   */
  case class Success[+T](result: T, override val next: Input) extends ParseResult[T] {
    def map[U](f: T => U) = Success(f(result), next)
    def mapPartial[U](f: PartialFunction[T, U], error: T => String): ParseResult[U]
       = if(f.isDefinedAt(result)) Success(f(result), next)
         else Failure(error(result), next)

    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U]
      = f(result)(next)

    def filterWithError(p: T => Boolean, error: T => String, position: Input): ParseResult[T] =
      if (p(result)) this
      else Failure(error(result), position)

    def append[U >: T](a: => ParseResult[U]): ParseResult[U] = this

    def get: T = result

    /** The toString method of a Success. */
    override def toString = "["+next.pos+"] parsed: "+result

    val successful = true
  }


  /** A common super-class for unsuccessful parse results. */
  sealed abstract class NoSuccess(val msg: String, override val next: Input) extends ParseResult[Nothing] { // when we don't care about the difference between Failure and Error
    val successful = false

    def map[U](f: Nothing => U) = this
    def mapPartial[U](f: PartialFunction[Nothing, U], error: Nothing => String): ParseResult[U] = this

    def flatMapWithNext[U](f: Nothing => Input => ParseResult[U]): ParseResult[U]
      = this

    def filterWithError(p: Nothing => Boolean, error: Nothing => String, position: Input): ParseResult[Nothing] = this

    def get: Nothing = scala.sys.error("No result when parsing failed")
  }
  /** An extractor so `NoSuccess(msg, next)` can be used in matches. */
  object NoSuccess {
    def unapply[T](x: ParseResult[T]) = x match {
      case Failure(msg, next)   => Some((msg, next))
      case Error(msg, next)     => Some((msg, next))
      case _                    => None
    }
  }

  /** The failure case of `ParseResult`: contains an error-message and the remaining input.
   *  Parsing will back-track when a failure occurs.
   *
   *  @param msg    An error message string describing the failure.
   *  @param next   The parser's unconsumed input at the point where the failure occurred.
   */
  case class Failure(override val msg: String, override val next: Input) extends NoSuccess(msg, next) {
    /** The toString method of a Failure yields an error message. */
    override def toString = "["+next.pos+"] failure: "+msg+"\n\n"+next.pos.longString

    def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = { val alt = a; alt match {
      case Success(_, _) => alt
      case ns: NoSuccess => if (alt.next.pos < next.pos) this else alt
    }}
  }

  /** The fatal failure case of ParseResult: contains an error-message and
   *  the remaining input.
   *  No back-tracking is done when a parser returns an `Error`.
   *
   *  @param msg    An error message string describing the error.
   *  @param next   The parser's unconsumed input at the point where the error occurred.
   */
  case class Error(override val msg: String, override val next: Input) extends NoSuccess(msg, next) {
    /** The toString method of an Error yields an error message. */
    override def toString = "["+next.pos+"] error: "+msg+"\n\n"+next.pos.longString
    def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = this
  }

  def Parser[T](f: Input => ParseResult[T]): Parser[T]
    = new Parser[T]{ def apply(in: Input) = f(in) }

  /** The root class of parsers.
   *  Parsers are functions from the Input type to ParseResult.
   */
  abstract class Parser[+T] extends (Input => ParseResult[T]) {
    private var name: String = ""
    def named(n: String): this.type = {name=n; this}
    override def toString() = "Parser ("+ name +")"

    /** An unspecified method that defines the behaviour of this parser. */
    def apply(in: Input): ParseResult[T]

    def flatMap[U](f: T => Parser[U]): Parser[U]
      = Parser{ in => this(in) flatMapWithNext(f)}

    def map[U](f: T => U): Parser[U] //= flatMap{x => success(f(x))}
      = Parser{ in => this(in) map(f)}

    def filter(p: T => Boolean): Parser[T]
      = withFilter(p)

    def withFilter(p: T => Boolean): Parser[T]
      = Parser{ in => this(in) filterWithError(p, "Input doesn't match filter: "+_, in)}

    // no filter yet, dealing with zero is tricky!

    def append[U >: T](p0: => Parser[U]): Parser[U] = { lazy val p = p0 // lazy argument
      Parser{ in => this(in) append p(in)}
    }

    /** A parser combinator for sequential composition.
     *
     * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
     *
     * @param q a parser that will be executed after `p` (this parser)
     *          succeeds -- evaluated at most once, and only when necessary.
     * @return a `Parser` that -- on success -- returns a `~` (like a `Pair`,
     *         but easier to pattern match on) that contains the result of `p` and
     *         that of `q`. The resulting parser fails if either `p` or `q` fails.
     */
    def ~ [U](q: => Parser[U]): Parser[~[T, U]] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield new ~(a,b)).named("~")
    }

    /** A parser combinator for sequential composition which keeps only the right result.
     *
     * `p ~> q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
     *
     * @param q a parser that will be executed after `p` (this parser)
     *        succeeds -- evaluated at most once, and only when necessary.
     * @return a `Parser` that -- on success -- returns the result of `q`.
     */
    def ~> [U](q: => Parser[U]): Parser[U] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield b).named("~>")
    }

    /** A parser combinator for sequential composition which keeps only the left result.
     *
     *  `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input
     *           left over by `p`.
     *
     * @note <~ has lower operator precedence than ~ or ~>.
     *
     * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
     * @return a `Parser` that -- on success -- returns the result of `p`.
     */
    def <~ [U](q: => Parser[U]): Parser[T] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield a).named("<~")
    }

    /** A parser combinator for alternative composition.
     *
     *  `p | q` succeeds if `p` succeeds or `q` succeeds.
     *   Note that `q` is only tried if `p`s failure is non-fatal (i.e., back-tracking is allowed).
     *
     * @param q a parser that will be executed if `p` (this parser) fails (and allows back-tracking)
     * @return a `Parser` that returns the result of the first parser to succeed (out of `p` and `q`)
     *         The resulting parser succeeds if (and only if)
     *         - `p` succeeds, ''or''
     *         - if `p` fails allowing back-tracking and `q` succeeds.
     */
    def | [U >: T](q: => Parser[U]): Parser[U] = append(q).named("|")

    /** A parser combinator for function application.
     *
     *  `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
     *
     * @param f a function that will be applied to this parser's result (see `map` in `ParseResult`).
     * @return a parser that has the same behaviour as the current parser, but whose result is
     *         transformed by `f`.
     */
    def ^^ [U](f: T => U): Parser[U] = map(f).named(toString+"^^")

    /** A parser combinator that changes a successful result into the specified value.
     *
     *  `p ^^^ v` succeeds if `p` succeeds; discards its result, and returns `v` instead.
     *
     * @param v The new result for the parser, evaluated at most once (if `p` succeeds), not evaluated at all if `p` fails.
     * @return a parser that has the same behaviour as the current parser, but whose successful result is `v`
     */
    def ^^^ [U](v: => U): Parser[U] =  new Parser[U] {
      lazy val v0 = v // lazy argument
      def apply(in: Input) = Parser.this(in) map (x => v0)
    }.named(toString+"^^^")

    /** A parser combinator for partial function application.
     *
     *  `p ^? (f, error)` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
     *  in that case, it returns `f` applied to the result of `p`. If `f` is not applicable,
     *  error(the result of `p`) should explain why.
     *
     * @param f a partial function that will be applied to this parser's result
     *          (see `mapPartial` in `ParseResult`).
     * @param error a function that takes the same argument as `f` and produces an error message
     *        to explain why `f` wasn't applicable
     * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
     *         to the result. If so, the result will be transformed by `f`.
     */
    def ^? [U](f: PartialFunction[T, U], error: T => String): Parser[U] = Parser{ in =>
      this(in).mapPartial(f, error)}.named(toString+"^?")

    /** A parser combinator for partial function application.
     *
     *  `p ^? f` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
     *  in that case, it returns `f` applied to the result of `p`.
     *
     * @param f a partial function that will be applied to this parser's result
     *          (see `mapPartial` in `ParseResult`).
     * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
     *         to the result. If so, the result will be transformed by `f`.
     */
    def ^? [U](f: PartialFunction[T, U]): Parser[U] = ^?(f, r => "Constructor function not defined at "+r)

    // shortcuts for combinators:

    /** Returns `into(fq)`. */
    def >>[U](fq: T => Parser[U]) = flatMap(fq)

    /** Returns a parser that repeatedly parses what this parser parses.
     *
     *  @return rep(this)
     */
    def * = rep(this)

    /** Returns a parser that repeatedly (at least once) parses what this parser parses.
     *
     *  @return rep1(this)
     */
    def + = rep1(this)

    /** Returns a parser that optionally parses what this parser parses.
     *
     *  @return opt(this)
     */
    def ? = opt(this)

    /** Changes the failure message produced by a parser.
     *
     *  This doesn't change the behavior of a parser on neither
     *  success nor error, just on failure. The semantics are
     *  slightly different than those obtained by doing `| failure(msg)`,
     *  in that the message produced by this method will always
     *  replace the message produced, which is not guaranteed
     *  by that idiom.
     *
     *  For example, parser `p` below will always produce the
     *  designated failure message, while `q` will not produce
     *  it if `sign` is parsed but `number` is not.
     *
     *  {{{
     *  def p = sign.? ~ number withFailureMessage  "Number expected!"
     *  def q = sign.? ~ number | failure("Number expected!")
     *  }}}
     *
     *  @param msg The message that will replace the default failure message.
     *  @return    A parser with the same properties and different failure message.
     */
    def withFailureMessage(msg: String) = Parser{ in =>
      this(in) match {
        case Failure(_, next) => Failure(msg, next)
        case other            => other
      }
    }

  }

  /** A parser that matches only the given element `e`.
   *
   *  `elem(e)` succeeds if the input starts with an element `e`.
   *
   *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
   *  @return a `Parser` that succeeds if `e` is the next available input (and returns it).
   */
  def elem(e: Elem): Parser[Elem] = accept(e)

  /** A parser that matches only the given element `e`.
   *
   *  The method is implicit so that elements can automatically be lifted to their parsers.
   *  For example, when parsing `Token`s, `Identifier("new")` (which is a `Token`) can be used directly,
   *  instead of first creating a `Parser` using `accept(Identifier("new"))`.
   *
   *  @param e the `Elem` that must be the next piece of input for the returned parser to succeed
   *  @return a `tParser` that succeeds if `e` is the next available input.
   */

  implicit def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)("'"+e+"' expected but " + _ + " found")

  /** A parser matching input elements that satisfy a given predicate.
   *
   *  `acceptIf(p)(el => "Unexpected "+el)` succeeds if the input starts with an element `e` for which `p(e)` is true.
   *
   *  @param  err    A function from the received element into an error message.
   *  @param  p      A predicate that determines which elements match.
   *  @return        A parser for elements satisfying p(e).
   */
  def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser { in =>
    if (in.atEnd) Failure("end of input", in)
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure(err(in.first), in)
  }

  /** A parser that always fails.
   *
   * @param msg The error message describing the failure.
   * @return A parser that always fails with the specified error message.
   */
  def failure(msg: String) = Parser{ in => Failure(msg, in) }

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

    def continue(in: Input): ParseResult[List[T]] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
        case Success(x, rest) => elems += x ; applyp(rest)
        case e @ Error(_, _)  => e  // still have to propagate error
        case _                => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first(in) match {
      case Success(x, rest) => elems += x ; continue(rest)
      case ns: NoSuccess    => ns
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

      @tailrec def applyp(in0: Input): ParseResult[List[T]] =
        if (elems.length == num) Success(elems.toList, in0)
        else p0(in0) match {
          case Success(x, rest) => elems += x ; applyp(rest)
          case ns: NoSuccess    => ns
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
      case Success(_, _)  => Failure("Expected failure", in)
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
          else Failure("end of input expected", in1)
        case ns => ns
      }
    }

  /** Given a concatenation with a repetition (list), move the concatenated element into the list */
  def mkList[T] = (_: ~[T, List[T]]) match { case x ~ xs => x :: xs }

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

}
