package laika.parse.core

/** The root class of parsers.
  *  Parsers are functions from the Input type to ParseResult.
  */
abstract class Parser[+T] extends (ParserContext => ParseResult[T]) {

  import Parsers._

  private var name: String = ""
  def named(n: String): this.type = {name=n; this}
  override def toString() = "Parser ("+ name +")"

  /** An unspecified method that defines the behaviour of this parser. */
  def apply(in: ParserContext): ParseResult[T]

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
    def apply (in: ParserContext) = Parser.this(in) map (x => v0)
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
  def ^? [U](f: PartialFunction[T, U], error: T => String): Parser[U] = Parser { in =>
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
    *  TODO - only used in one place
    *
    *  @param msg The message that will replace the default failure message.
    *  @return    A parser with the same properties and different failure message.
    */
  def withFailureMessage(msg: String) = Parser { in =>
    this(in) match {
      case Failure(_, next) => Failure(Message(msg), next)
      case other            => other
    }
  }

}