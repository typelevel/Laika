
Laika's Parser Combinators
==========================

Since the 0.8 release in 2018 Laika comes with its own parser combinator library.

The decision was based on the goal to find the sweet spot between ease of use, flexibility and performance.
It's to a large degree a general purpose parser library for character input, 
but also has a few aspects tailored for Laika's main requirements: 
parsing of text markup which is a multi-pass process and providing a maximum of flexibility for user provided
extensions of the markup syntax.

The library is currently part of the `laika-core` artifact, but has the potential to become a separate micro-lib,
if some users voice an interest in using it without Laika's other features.


The Parser Trait
----------------

A parser instance is invoked at a particular offset into an input and performs the following tasks:

1) produce a result or fail with a message

2) optionally consume some of the input

This is encapsulated in the single abstract method of the `Parser` trait:

```scala
trait Parser[T] {
  def parse (in: ParserContext): Parsed[T]
}
```

The `ParserContext` contains an API for reading from the input at the current offset and for capturing or consuming
some of the input.

The returned result of type `Parsed[T]` is a little ADT with the following types:

```scala
case class Success[+T] (result: T, next: ParserContext) extends Parsed[T]

case class Failure (msg: Message, next: ParserContext) extends Parsed[Nothing]
```

In case of success the result will be returned alongside a new `ParserContext` that may have consumed
some of the input.
Consuming input is optional as some parsers only look ahead on the input to check a precondition.

In case of an error the returned value will contain a (lazily constructed) error message 
and a `ParserContext` which will usually be at the same offset as the context passed in,
so that subsequent parsers can try to read from the same location.

The trait is mostly shown for providing some background about the most basic building block of the combinator library.
You will rarely create an implementation of `Parser` yourself as the existing building blocks cover a lot of scenarios
already.


Parsing Text
------------

One of the basic building blocks for parsers are the text parsers provided by the library.

They deliberately do not integrate with or otherwise use regular expressions and prefer a combinator DSL instead.
While being significantly more verbose, combinators are a more type-safe, composable and also often more legible
way to define text parsers, in particular with larger, more complex expressions.

For defining parsers you need the following two imports:

```scala
import laika.parse.builders._
import laika.parse.implicits._
```

The following section demonstrate some of the most commonly used text parsers.
For the full API see @:api(laika.parse.text.TextParsers$).


### Literal Text

A literal parser only matches on an exactly identical input and is often used for parsing keywords or 
delimiters consisting of one or more characters.

`literal("class")` for example only matches on the `class` keyword.

In many cases declaring the literal parser explicitly can be skipped.
The imports shown above contain extension methods that allow the use of raw strings 
where otherwise a `Parser` type is expected.

`"[" ~ anyNot(']') ~ "]"` is a shortcut for `literal("[") ~ anyNot(']') ~ literal("]")`.


### Character Groups

To parse a range of characters you can either provide a set of matching characters or a predicate:

* `anyOf('*', '+', '-')` reads zero or more characters that match any of the three specified symbols.
  E.g. reading `**--**..` would provide the result `**--**`.
  
* `anyNot('*', '+', '-')` is the opposite, it reads zero or more characters that do not match the specified symbols.
  E.g. reading `..,,**--**` would provide the result `..,,`.
  
* `anyWhile(_.isUpper)` reads zero or more upper case characters. Any predicate `Char => Boolean` can be specified.
  
* The `range` helper constructs set of characters, e.g. `anyOf(range('a','z'))`.

* There are predefined character groups for common scenarios, e.g. `anyOf(CharGroup.hexDigit)`.

For all the shown parsers the result may be empty and the parser will always succeed.
There are ways to set conditions on the length of the input shown in the next section.


### Length Constraints

All character parsers come with `min`, `max` and `take` methods to specify constraints on the length of the result.

* `anyOf(CharGroup.alphaNum).min(3)` expects 3 or more matching characters. 
  It fails when it matches on fewer than that.
  
* `anyOf(CharGroup.alphaNum).min(3)` expects 0 to 3 characters. 
  It always succeeds. After 3 characters are read it simply ignores any further matching input.

* `anyOf(CharGroup.alphaNum).take(3)` is a shortcut for `anyOf(CharGroup.alphaNum).min(3).max(3)`.

* `someOf`, `someNot` and `someWhile` builders are shortcuts for `anyOf`, `anyNot` and `anyWhile`
  that expect at least one matching character. 
  E.g. `someOf(CharGroup.alphaNum)` is equivalent to `anyOf(CharGroup.alphaNum).min(1)`. 
  That's not that many characters saved, but used so frequently that it's worthwhile.
  
* Likewise `oneOf`, `oneNot` and `oneIf` are shortcuts for parsers that parse exactly one character.
  E.g. `oneOf(CharGroup.alphaNum)` is equivalent to `anyOf(CharGroup.alphaNum).take(1)`.

* The `count` method creates a parser that provides the number of characters read as the result.
  Commonly used for parsing decoration where the length is significant, 
  e.g. for Markdown headers starting with one or more `#`: `someOf('#').count`.


Combinators
-----------

So far we were only parsing a single range of input based a condition or set of accepted input characters.
In every kind of real-world scenario you would need to combine these low-level parsers to larger
constructs.


### Concatenation

The `~` combines the result of two parsers and only succeeds if both of them succeed:

```scala
val p = oneOf('$','_') ~ someOf(range('a', 'z'))
```

The above parser expect exactly one occurrence of either `$` or `_`, followed by one or more occurrences of
a lowercase letter.

The result will be `String ~ String`, where `~` is a case class that allows to map on the result with the
same symbol:

```scala
p.map { case firstChar ~ lowerCaseLetter => Seq(firstChar, lowerCaseLetter) }
```

In many cases we are only interested in one of the results of a concatenation, 
when some of the results are known for example.
The `~>` combinator ignores the left result, `<~` ignores the right one: 

```scala
val p = "<" ~> someOf(range('a', 'z')) <~ ">"
```

The result of this parser will be `String` as the first and last result will be ignored.


### Alternatives

Two parsers can be defined to be tried as alternatives, 
where the second will only be invoked if the first parser fails:

```scala
("\"" ~> anyBut('"') <~ "\"") | someBut(' ')
```

The example above parses either text enclosed in double quotes or a string without spaces. 
The type of the result will be the lowest upper bound of the individual results (like with `Option.orElse`).

The resulting parser will succeed if either the first or the second parser succeeds.
If both of them fail, the returned error will be the one from the parser which successfully read the most characters.
This will of course also apply to longer chains of alternatives like `a | b | c | d`.
In many cases this will be the error message that is the most helpful for the user.


### Repetitions

The same parser can be invoked repeatedly, while collecting all individual results on the way:

```scala
someOf(CharGroup.alphaNum) ~ (',' ~> someOf(CharGroup.alphaNum).rep)
```

The above reads a non-empty sequence of alphanumerical characters followed by zero or more repetitions
of a comma followed by another sequence of characters.
The result will be `String ~ Seq[String]`

This pattern is so common that there is also a shortcut for repeating with a separator:

```scala
someOf(CharGroup.alphaNum).rep(',')
```

This parser's behaviour is identical to the previous one.

The number of repetitions can be further constrained:

```scala
someOf(CharGroup.hexDigit).max(4)
```

The above reads between 1 to 4 hexadecimal digits.

When using the `min` constraint the parser will fail when it does not reach the specified minimum number of repetitions,
while the `max` constraint will always succeed and simply ignore subsequent repetitions.

There is also a shortcut called `.take(4)` which is identical to `.min(4).max(4)`.


Producing Results
-----------------

All of our preceding examples defined lower-level text parsers that produce a `String` result
or `String ~ String` in case of concatenation and `Seq[String]` in case of repetition.

In many real-world scenarios parsers are used to build up a form of AST of the parsed input.
All concrete parsers in Laika do this, the ones of text markup, HOCON and CSS all produce a model representing the input.

First there is the classic map:

```scala
def map[U] (f: T => U): Parser[U]
```

The API also offers methods that allow to check pre-conditions on the result, 
potentially causing the parser to fail if they are not met:

```scala
def evalMap [U] (f: T => Either[String, U]): Parser[U]
def collect [U] (f: PartialFunction[T, U]): Parser[U]
```

While `evalMap` maps to an `Either` where a `Left` result will cause the parser to fail,
`collect` applies a partial function and causes the parser to fail if the function is not defined for the result.

```scala
someOf(CharGroup.digit).evalMap { res =>
  val num = res.toInt
  Either.cond(num % 3 == 0, num, "Number must be divisible by three")
}
```

The example above creates a parser that reads any number divisible by 3 or fails otherwise.

You can also chain parsers with `flatMap`.
The example parses a start delimiter out of 3 options and then looks for a matching delimiter to close the span:

```scala
oneOf('*', '-', '+').flatMap { res =>
  someBut(res.charAt(0)) <~ literal(res)
}
```

The second parser will receive the result of the first parser and will continue parsing on the input left over
by the first.

You can also ignore the original result of a parser and hard-code a result that should be used if the parser succeeds:

```scala
case object Fence

literal("```").as(Fence)
```

Another option is to ignore the results of a concatenation and instead use the entire consumed input as a result:

```scala
("\"" ~> anyBut('"') <~ "\"").source
```

This is usually more convenient in all cases where the result you would produce would have been the string concatenation
of the individual results anyway. 
Laika's own parsers use this very frequently.

Finally, while all the methods shown so far are available for all kinds of `Parser[T]`,
the `laika.parse.implicits._` import also provides a few convenient shortcuts for parsers of a certain result type.

A result of concatenating a single result with a repetition can be combined into a single list with `concat` for example:

```scala
someOf(CharGroup.alphaNum) ~ (',' ~> someOf(CharGroup.alphaNum).rep).concat
```

This will turn a parser with the result `String ~ Seq[String]` into a parser for `Seq[String]`.

There are similar variants for results of type `Seq[T] ~ Seq[T]` of various arities.


Delimiters
----------

In many cases the task of a parser is not only to verify whether the input at the current position matches
a particular predicate, but also whether the surrounding characters meet certain criteria.

Let's take the parsing of an emphasized span in text markup as an example. 
Most languages define exceptions where the span `*here*` is not parsed as emphasized, 
for example when it appears in the middle of a word like `in*this*example`. 

Laika's parser combinators come with convenient helpers to check conditions on preceding and following characters
without consuming them.
Doing this manually with existing low-level combinators is possible, but much more verbose and error-prone.

A simplified definition of such a parser for an emphasized span could look like this:

```scala
val letterOrDigit: Char => Boolean = { c => 
  Character.isDigit(c) || Character.isLetter(c)
}
val start = delimiter("*").prevNot(letterOrDigit)
val end   = delimiter("*").nextNot(letterOrDigit)

start ~> someNot('*') <~ end 
  
```

Instead of just using a literal parser for the `"*"` we use the `delimiter` parser and its integrated
shortcuts to check conditions before and after the delimiter.

Instead of just parsing the delimiter itself, an even more powerful variant parses a span of text until
a specified delimiter is seen.

The last line in the previous example can be shortened to:

```scala
start ~> delimitedBy(end)
```

But this variant does not only save a bit of typing, it also comes with additional functionality
for defining the parsed span.
One of them is the option to specify conditions under which the parser should fail,
even if it would later see the given delimiter.

Let's assume we want an emphasized span that is not allowed to span multiple lines.
We can express this with `failOn`:  

```scala
start ~> delimitedBy(end).failOn('\n')
```

Other methods are `acceptEOF` which tells the parser to succeed if either the specified delimiter or the end of the
input is reached or `nonEmpty` which causes the parser to fail if the delimiter is seen before consuming any input.


Parsing Nested Spans
--------------------

The last feature area of the parser combinators we are going to look at is the recursive parsing of nested spans,
which is at the heart of inline text markup parsing and another area where the library takes away a lot of 
the boilerplate you'd have to write with simple low-level combinators.

For a real world example, let's assume we want to parse markup that allows a Markdown-like link syntax,
between brackets, with potential markup inside. 
We don't allow this syntax to span multiple lines.

This is how a simplified implementation could look like:

```scala
val nestedSpanParsers: Seq[PrefixedParser[Span]] = ???
val linkSpanParser = delimitedBy("]").failOn('\n')

"[" ~> InlineParsers.spans(linkSpanParser).embedAll(nestedSpanParsers)
```

We skip the definitions of all the nested parsers, with typical markup parsing this would be a handful or a dozen.
In the second line we define the parser for the link span itself, using the `delimitedBy` builder 
which we introduced in the previous section.

Finally we pass this parser to the `InlineParsers.spans` constructor and then tell it to embed all the nested
span parsers we defined.

The resulting parser will:

* Fail when it sees a newline before seeing the end delimiter.

* Suspend parsing of this span when a pattern signalling the start of a nested span is seen.
  It will then parse the nested span, add it to the result and resume parsing of the top span.
  
* It will finally succeed and provide the result (as a `Seq[Span]`) when it sees the end delimiter.

There is an alternative constructor `InlineParsers.text` where the result is `String` and not `Seq[Span]`.
It is less often used, but comes in handy when you want to support something like text between parenthesis
that should not stop when a closing paren of a nested pair is seen.

The example above is applicable for a scenario where the author of the code has full control over the
kind of nested spans which are allowed. 
In Laika's text markup support this is not the case, as the user can install markup extensions.
For this scenario the nested parsers need to be injected from the environment,
which is demonstrated in [Recursive Parsing] in the chapter on writing parser extensions for markup.


Performance Optimizations
-------------------------

The final example in the previous section demonstrated the principles behind parsing inline markup.
These kind of parsers are quite challenging to optimize as they are at a hotspot of markup parsing and
recurse between user-provided and library-provided parsers which makes it harder to optimize based on static analysis.


### Inline Parsing Challenges

Laika has therefore chosen an approach where the optimization happens at parser construction time,
at a point where all participating parsers are known.

The key point of the optimization is to avoid a naive combinator-based approach for logic that has to
execute for nearly every character of the input stream.
A naive approach would try something like `spanA | spanB | spanC | spanD | plainText` on each character,
where `spanA` end so on are the supported inline constructs.
It would need to try a long chain of parsers where in probably 95% of cases it would need to fall back
to the last one: read the input as plain text, the most likely outcome.

On top of the long list of choices, the parser has to look for other conditions, too,
as shown in the previous section: characters that cause the current span to fail and those who terminate it.

Laika combines all these aspects into a single quick check, a simple array lookup that first determines
whether the current input character is an "interesting" one or just a character to consume and move on.
Only when this condition is met, the actual parsers are invoked.


### The PrefixedParser trait

To facilitate this at runtime, the library comes with an optimizable sub-trait of `Parser[T]` called
`PrefixedParser[T]`. 
On top of the actual parsing logic it encapsulates a separate condition for the first character.
With normal parsers this aspect is opaque. 

Some API entry points in hotspots only accept such a kind of parser and not the base trait,
when they are in a hotspot of the parsing logic.
You normally do not have to worry about this, as most of the likely candidates for defining the start 
condition of an inline construct would satisfy the condition.

But when you get a compiler error `expected PrefixedParser[T], got Parser[T]` you know why.
You tried to pass a non-optimizable parser where the library does not accept it.
This prevents severe performance degradations just because an extension has been installed.

As a rule of thumb, all text parsers that do not A) build on negation (like `oneNot` or `someNot`)
or B) allow for empty results like all `anyOf`, `anyNot` or `anyWhile` satisfy this condition.

This design follows the principle of enabling decent performance, but not at the cost of ease of extensibility,
one key capability of the library.
