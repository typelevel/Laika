Foo [bar] [1].

Foo [bar][1].

Foo [bar]
[1].

[1]: ext:/url/  "Title"


With [embedded [brackets]] [b].


Indented [once][].

Indented [twice][].

Indented [thrice][].

Indented [four][] times.

 [once]: ext:/url

  [twice]: ext:/url

   [thrice]: ext:/url

    [four]: /url


[b]: ext:/url/

* * *

[this] [this] should work

So should [this][this].

And [this] [].

And [this][].

And [this].

But not [that] [].

Nor [that][].

Nor [that].

[Something in brackets like [this][] should work]

[Same with [this].]

In this case, [this](ext:/somethingelse/) points to something else.

Backslashing should suppress \[this] and [this\].

[this]: ext:/foo


* * *

Here's one where the [link
breaks] across lines.

Here's another where the [link 
breaks] across lines, but with a line-ending space.


[link breaks]: ext:/url/
