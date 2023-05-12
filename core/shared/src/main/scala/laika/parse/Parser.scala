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

package laika.parse

import laika.ast.~
import laika.parse.combinator.Parsers.opt
import laika.parse.combinator.Repeat
import laika.parse.text.TextParsers

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**  The abstract base for all parser implementations.
  *
  *  Contains the main `parse` function as well as various
  *  combinator function to create a new parser based on this one.
  *
  *  @author Jens Halm
  */
abstract class Parser[+T] {

  /** Parses the string content in the specified context
    * and returns the result.
    *
    * This is the only abstract method in `Parser` that
    * concrete implementations need to implement.
    */
  def parse(in: SourceCursor): Parsed[T]

  /** Parses the specified string and returns the result.
    */
  def parse(in: String): Parsed[T] = parse(SourceCursor(in))

  /** Builds a new parser by applying the specified function
    * to the result of this parser and subsequently applying
    * the parser returned by that function to the input left
    * over by this parser.
    */
  def flatMap[U](f: T => Parser[U]): Parser[U] = Parser { in =>
    parse(in) match {
      case Success(result, next) => f(result).parse(next)
      case e: Failure            => e
    }
  }

  /** Builds a new parser by applying the specified function
    * to the result of this parser.
    */
  def map[U](f: T => U): Parser[U] = Parser { in => parse(in).map(f) }

  /**  Applies the specified parser when this parser fails.
    *
    *  `a orElse b` succeeds if either of the parsers succeeds.
    *
    *  In case both parsers fail, the `Failure` instance will
    *  be from the parser with the most successfully read characters.
    *  In the case of multiple failures having the same number of characters,
    *  the one with the highest precedence (this parser) will be chosen.
    *
    *  Implementation note:
    *  The parameter is by-name to allow the definition of
    *  recursive parsers. In contrast to the former SDK
    *  parser combinators this is the only place where
    *  a parser with a by-name parameter is used whereas
    *  in all other places the additional cost is avoided.
    */
  def orElse[U >: T](p0: => Parser[U]): Parser[U] = {
    lazy val alt = p0
    Parser { in =>
      parse(in) orElse alt.parse(in)
    }
  }

  /** Attempts to parse the specified literal string from the input left over
    * by this parser and combines the two results.
    *
    *  `a ~ b` only succeeds if both parsers succeed, with the results
    *  in a wrapper class named `~` for convenient pattern matching:
    *
    *  {{{
    *    a ~ b ~ c ^^ {
    *      case a ~ b ~ c => processResult(a, b, c)
    *    }
    *  }}}
    */
  def ~ (value: String): Parser[T ~ String] = this.~(TextParsers.literal(value))

  /**  Applies the specified parser to the input left over by this parser
    *  and combines the two results.
    *
    *  `a ~ b` only succeeds if both parsers succeed, with the results
    *  in a wrapper class named `~` for convenient pattern matching:
    *
    *  {{{
    *    a ~ b ~ c ^^ {
    *      case a ~ b ~ c => processResult(a, b, c)
    *    }
    *  }}}
    */
  def ~ [U](p: Parser[U]): Parser[T ~ U] = Parser { ctx =>
    parse(ctx) match {
      case Success(aRes, next) => p.parse(next).map(bRes => new ~(aRes, bRes))
      case f: Failure          => f
    }
  }

  /** Attempts to parse the specified literal string from the input left over by this parser,
    * but only keeps the right result.
    *
    *  `a ~> b` only succeeds if both parsers succeed.
    */
  def ~> (value: String): Parser[String] = this.~>(TextParsers.literal(value))

  /**  Applies the specified parser to the input left over by this parser,
    *  but only keeps the right result.
    *
    *  `a ~> b` only succeeds if both parsers succeed.
    */
  def ~> [U](p: Parser[U]): Parser[U] = Parser { ctx =>
    parse(ctx) match {
      case Success(_, next) => p.parse(next)
      case f: Failure       => f
    }
  }

  /** Attempts to parse the specified literal string from the input left over by this parser,
    * but only keeps the left result.
    *
    * `a <~ b` only succeeds if both parsers succeed.
    */
  def <~ (value: String): Parser[T] = this.<~(TextParsers.literal(value))

  /**  Applies the specified parser to the input left over by this parser,
    *  but only keeps the left result.
    *
    *  `a <~ b` only succeeds if both parsers succeed.
    */
  def <~ [U](p: Parser[U]): Parser[T] = Parser { ctx =>
    parse(ctx) match {
      case Success(aRes, next) => p.parse(next).map(_ => aRes)
      case f: Failure          => f
    }
  }

  /**  Applies the specified parser when this parser fails.
    *
    *  `a | b` succeeds if either of the parsers succeeds.
    *
    *  Implementation note:
    *  The parameter is by-name to allow the definition of
    *  recursive parsers. In contrast to the former SDK
    *  parser combinators this is the only place where
    *  a parser with a by-name parameter is used whereas
    *  in all other places the additional cost is avoided.
    */
  def | [U >: T](p: => Parser[U]): Parser[U] = orElse(p)

  /**  Attempts to parse the specified literal string when this parser fails.
    *
    *  `a | b` succeeds if either of the parsers succeeds.
    */
  def | (value: String)(implicit ev: T <:< String): Parser[String] =
    map(ev).orElse(TextParsers.literal(value))

  /**  A synonym for `map`, allowing the grammar to be declared in a concise way.
    */
  def ^^ [U](f: T => U): Parser[U] = map(f)

  /**  Returns a parser that ignores the result of this parser (if it succeeds)
    *  and returns the specified result instead.
    *
    *  Subclasses may override this method to avoid any expensive
    *  result processing.
    */
  def as[U](v: => U): Parser[U] = new Parser[U] {
    lazy val v0                 = v
    def parse(in: SourceCursor) = Parser.this.parse(in) map (_ => v0)
  }

  /** Discards the result of a successful parser.
    */
  def void: Parser[Unit] = map(_ => ())

  /** Returns a parser that applies a partial function to the result of this parser.
    *
    * `p.collect(f)` succeeds if `p` succeeds and `f` is defined at the result of `p`,
    * In that case it returns `f` applied to the result of `p`.
    *
    * @param f a partial function that will be applied to this parser's result.
    * @param error an optional function that takes the same argument as `f` and produces an error message.
    */
  def collect[U, V >: T](
      f: PartialFunction[T, U],
      error: V => String = (r: V) => s"Constructor function not defined at $r"
  ): Parser[U] = {
    val msg: V => Message = Message.forRuntimeValue(error)
    Parser { in =>
      parse(in) match {
        case Success(result, next) =>
          if (f.isDefinedAt(result)) Success(f(result), next)
          else Failure(msg(result), in, next.offset)
        case f: Failure            => f
      }
    }
  }

  /**  Returns a parser that applies a function to the result of this parser producing an `Either`
    *  where `Left` is interpreted as failure. It is an alternative to `^?` for scenarios
    *  where the conditional check cannot be easily performed in a pattern match.
    *
    *  `p.evalMap(f)` succeeds if `p` succeeds and `f` returns a `Right` when applied to the result
    *  of `p`.
    */
  def evalMap[U](f: T => Either[String, U]): Parser[U] = Parser { in =>
    parse(in) match {
      case Success(result, next) =>
        f(result).fold(
          msg => Failure(Message.fixed(msg), in, next.offset),
          res => Success(res, next)
        )
      case f: Failure            => f
    }

  }

  /** Operator synonym for `flatMap`.
    */
  def >> [U](fq: T => Parser[U]): Parser[U] = flatMap(fq)

  /**  Returns a parser that repeatedly applies this parser.
    *  The returned parser offers an API to specify further constraints
    *  like `min` or `max`.
    */
  def rep: Repeat[T] = new Repeat(this)

  /** Returns a parser that repeatedly applies this parser with the specified
    * separator parser between those invocations.
    *
    * `p.rep(sep).min(1)` is equivalent to `(p ~ (sep ~> p).rep).concat`.
    *
    * The returned parser offers an API to specify further constraints
    * like `min` or `max`.
    */
  def rep(separator: Parser[Unit]): Repeat[T] = new Repeat(this, sep = Some(separator))

  /** Returns a parser that repeatedly applies this parser with the specified
    * separator string between those invocations.
    *
    * `p.rep(sep).min(1)` is equivalent to `(p ~ (sep ~> p).rep).concat`.
    *
    * The returned parser offers an API to specify further constraints
    * like `min` or `max`.
    */
  def rep(separator: String): Repeat[T] =
    new Repeat(this, sep = Some(TextParsers.literal(separator).void))

  /**  Returns a parser that repeatedly applies this parser.
    *  It will always succeed, potentially with an empty list as the result.
    */
  def * : Repeat[T] = new Repeat(this)

  /** Returns a parser that repeatedly applies this parser (at least once).
    */
  def + : Repeat[T] = rep.min(1)

  /** Returns a parser that optionally parses what this parser parses.
    */
  def ? : Parser[Option[T]] = opt(this)

  /** Returns a parser that repeatedly applies this parser until either this parser fails or the specified
    * end condition is met.
    * The end condition will be applied after each successful invocation of this parser.
    *
    * The result of the returned parser is a tuple consisting of the list containing the
    * result of the invocations of this parser plus the result of the end condition.
    * The latter is returned as an `Option` as it might be empty when the parsing finished because of this parser failing.
    *
    * Note that it is more convenient to include the end condition in the repeating parser itself and use
    * the simpler `rep` method.
    * This combinator is an alternative if you need to know the result of the end condition.
    */
  def repUntil[U](endCondition: Parser[U]): Parser[(List[T], Option[U])] = Parser { in =>
    val elems = new ListBuffer[T]

    val combined = this ~ opt(endCondition)

    @tailrec
    def loop(input: SourceCursor): Parsed[(List[T], Option[U])] =
      combined.parse(input) match {
        case Success(result ~ Some(endCond), rest)    =>
          elems += result
          Success((elems.toList, Some(endCond)), rest)
        case Success(laika.ast.~(result, None), rest) =>
          elems += result
          loop(rest)
        case _: Failure                               =>
          Success((elems.toList, None), input)
      }

    loop(in)
  }

  /** Returns a parser that invokes the specified function repeatedly,
    * passing the result of this parser if it succeeds, to produce new
    * parsers that get applied until one of them fails.
    *
    * The result of the returned parser is a list containing the
    * result of this parser (if it succeeds) plus the results of
    * successful invocations of the parsers returned by the specified
    * function.
    */
  def repWith[U >: T](next: U => Parser[U]): Parser[List[U]] = Parser { in =>
    val elems = new ListBuffer[U]

    @tailrec
    def parse(input: SourceCursor, p: Parser[U]): Parsed[List[U]] =
      p.parse(input) match {
        case Success(result, rest) =>
          elems += result
          val newParser = next(result)
          parse(rest, newParser)
        case _: Failure            => Success(elems.toList, input)
      }

    parse(in, this)
  }

  /** Handle any error, potentially recovering from it, by mapping it to a new parser that
    * will be applied at the same starting position than the failing parser.
    *
    * This is similar to the `orElse` or `|` method, but allows the alternative
    * parser to inspect the error of the preceding one.
    *
    * @see [[recoverWith]] to recover from only certain errors.
    */
  def handleErrorWith[U >: T](f: Failure => Parser[U]): Parser[U] = Parser { in =>
    parse(in) match {
      case error: Failure => f(error).parse(in)
      case other          => other
    }
  }

  /** Handle certain errors, potentially recovering from it, by mapping them to a new parser that
    * will be applied at the same starting position than the failing parser.
    *
    * @see [[handleErrorWith]] to handle any/all errors.
    */
  def recoverWith[U >: T](pf: PartialFunction[Failure, Parser[U]]): Parser[U] =
    handleErrorWith(e => pf.applyOrElse[Failure, Parser[U]](e, { f => Parser[U] { _ => f } }))

  /**  Changes the failure message produced by a parser.
    */
  def withFailureMessage(msg: String): Parser[T] = Parser { in =>
    parse(in) match {
      case Failure(_, next, maxOff) => Failure(Message.fixed(msg), next, maxOff)
      case other                    => other
    }
  }

  /** Provides the result of this parser together with a cursor over the input,
    * capturing the consumed source string and its position within the root input.
    * Use `cursor` if you do not need access to the actual result.
    *
    * This is required for parsers that create AST nodes that need to be resolved in a rewrite step
    * and need to report the source location in case of failure.
    * It is also required when passing a result of a first-pass parser to a recursive parser
    * to preserve line positions.
    */
  def withCursor: Parser[(T, SourceFragment)] = Parser { in =>
    parse(in) match {
      case f: Failure            => f
      case Success(result, next) =>
        val consumed = in.capture(next.offset - in.offset)
        Success((result, LineSource(consumed, in)), next)
    }
  }

  /** Provides a cursor over the input consumed by this parser while discarding the actual result.
    * Use `withCursor` if you also need access to the result.
    *
    * This is required for parsers that create AST nodes that need to be resolved in a rewrite step
    * and need to report the source location in case of failure.
    * It is also required when passing a result of a first-pass parser to a recursive parser
    * to preserve line positions.
    */
  def cursor: Parser[SourceFragment] = withCursor.map(_._2)

  /** Retrieves the part of the input consumed by this parser while discarding the result.
    *
    * This is useful in scenarios where many string-based parsers are combined and produce a deeply nested result
    * like `String ~ Option[String] ~ List[String]` where it would require some boilerplate to concatenate the results.
    * Using the source method, the entire text consumed by this combination of parsers will be returned.
    *
    * If you also need the position within the input or need to pass the result to a recursive parser manually,
    * use the `cursor` method instead.
    */
  def source: Parser[String] = withCursor.map(_._2.input)

  /** Returns a parser that produces the number of characters
    * consumed by this parser while discarding the original result.
    */
  def count: Parser[Int] = withCursor.map(_._2.length)

}

/** Companion factory for creating new parser instances.
  */
object Parser {

  /** Builds a new parser based on the specified function
    * that implements the behaviour of the parser.
    */
  def apply[T](f: SourceCursor => Parsed[T]): Parser[T] = new Parser[T] {
    def parse(source: SourceCursor) = f(source)
  }

}
