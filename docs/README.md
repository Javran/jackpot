# Implementation notes

For now this directory serves as internal documents addressing various bits of
the implementation details.

## Major parsing rounds

A parsing round is conceptually a complete scan of input stream,
whether stream elements are bytes, escaped characters or tokens.

Keep that in mind however that we don't necessarily need to keep the
complete stream of data available - one round might produce some data,
which are then immediately consumed by next round.

### Preprocessing

This round performs `String`-based transformation to be compliation with "3.2. Lexical Translations".
In particular,
we performs Unicode escape and normalizes line terminators to `\n`,
and removes the optional ASCII `SUB` character, which is allowed at the end of input stream.

### Lexer Execution

This round performs lexical analysis via Alex-generated lexer code,
regular expressions are recognized from this round, which in turn triggers `AlexAction`
to produce tokens.

### (Planned) Literal Bound Check

This round rules out numeric literals that does not fit into their intended types.

## `Lexer.x` accepts a wider range of input that language itself

This decision is intentional - we want the lexer to have a quick pass and
dispatch results to Alex actions, in which we have full power of Haskell available
to do fancy stuff and potentially improve error messages.

In addition regular expression rules are not that maintainable.

## Regular expressions

There are some long and complicated regular expressions (REs) in `Lexer.x`,
those are accomplished thanks to [fsm2regex](http://ivanzuzak.info/noam/webapps/fsm2regex/),
which is a website that allows you to write finite state machine (FSM)
and convert them to REs.

Few limitation of that tool apply. Notably characters are limited - but this limitation
is easy to work around as long as we only have few of them.
The trick here is to break down all input symbols into disjoints sets
and assign one unique symbol for each set.
For example, if the RE works on `\`, `"` and any other characters other than those,
we just assign `a` to set `{'}`, `b` to set `{"}` and `c` to `all symbols - {'"}`.

Also note that the RE notation is different: `+` on that website means choice.

### On recognizing `TextBlock`

FSM:

```
#states
s0
s1
s2
s3
s4
s5
s6
s7
#initial
s0
#accepting
s6
#alphabet
q
s
z
#transitions
s0:q>s1
s1:q>s2
s2:q>s3
s3:q>s4
s4:q>s5
s5:q>s6
s3:z>s3
s4:z>s3
s5:z>s3
s3:s>s7
s4:s>s7
s5:s>s7
s7:q>s3
s7:s>s3
s7:z>s3
```

Alphabet:

- `q`: `"`
- `s`: `\`
- `z`: anything other than `"` or `\`.

Visualization:

![fsm-TextBlock](/docs/imgs/fsm-TextBlock.png)

Regular expression:

```
qqq(z+s(q+s+z)+q(z+s(q+s+z)+q(z+s(q+s+z))))*qqq
```

### On recognizing `TraditionalComments`

FSM:

```
#states
s0
s1
s2
s3
s4
#initial
s0
#accepting
s4
#alphabet
a
b
c
#transitions
s0:a>s1
s1:b>s2
s2:b>s3
s3:a>s4
s2:c>s2
s2:a>s2
s3:c>s2
s3:b>s3
```


Alphabet:

- `a`: `/`
- `b`: `*`
- `c`: anything other than `/` or `*`.

Visualization:

![fsm-TraditionalComments](/docs/imgs/fsm-TraditionalComments.png)

Regular expression:

```
ab(c+a+bb*c)*bb*a
```

## Regarding `$JavaIdentifierStart` and `$JavaIdentifierPart`

Note that the actual names in `Lexer.x` are: `$JavaIdentifierStartLite` and `$JavaIdentifierPartLite`.

The difference in names hints that the lexing rule is relaxed to allow all non-ASCII characters and
we rely on Alex action, which in turn calls
`PlatformFunction.{isJavaIdentifierStart,isJavaIdentifierPart}` to actually recognize the language.

While it is totally possible to correctly recognize Java identifiers from `Lexer.x`,
it would result in [some unpleasant chunks](https://github.com/Javran/jackpot/blob/bc2c02be271675f8dc16c6bda86eff2aa7fb1e82/src/Language/Java/Lexical/Lexer.x#L31-L32) in `Lexer.x` that I really don't like.

### Correctness

This does not have correctness implication, as from Java spec "§3.1 Unicode":

> Except for comments (§3.7), identifiers (§3.8, and the contents of character literals, string literals, and text blocks (§3.10.4, §3.10.5, §3.10.6), all input elements (§3.5) in a program are formed only from ASCII characters (or Unicode escapes (§3.3) which result in ASCII characters).

Given correct lexing of comments, character / string literals, we can safely assume
that all other non-ASCII characters are part of an identifier, consume it and verify its validity
as Java identifier later.

### Unicode 13.0.0

Quoting from Java spec "§3.1 Unicode":

> Upgrades to newer versions of the Unicode Standard occurred in ...
> and Java SE 15 (to Unicode 13.0).

However, we cannot rely on `Data.Char.generalCategory` provided by `base` package,
as it relies on UnicodeData [generated and shiped with GHC](https://github.com/ghc/ghc/blob/master/libraries/base/include/WCsubst.h).

To untie this parser from GHC's Unicode version, we are using [unicode-general-category](https://github.com/Javran/unicode-general-category) instead.

### Alex macro

There are discussions about [having Alex macros to handle general category](https://github.com/simonmar/alex/issues/126), which could be a nice alternative to my current solution.
But the discussion is still ongoing and it's unlikely to move forward very soon, given all the concerns.
