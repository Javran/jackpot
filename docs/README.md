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

This is an unfortunate bit: the platform library function
`Data.Char.generalCategory` relies on UnicodeData [generated and shiped with GHC](libraries/base/include/WCsubst.h). For this implementation to be truly independent of GHC version (as long as it's modern
to support all pragmas used by this library), that is out of the question.

In addition, on Haskell side, `Data.IntSet` and `Data.HashSet` simply doesn't have sufficiently good performance
even on small inputs comparing with that of Alex's - one can easily tell that by simply running
the test suite.

Therefore for now I've settled on dump that giant pile of unreadable garbage in Lexer.x and live with it.
This situation might change however, depending on where [this Alex issue](https://github.com/simonmar/alex/issues/126) is heading.

For now both `$JavaIdentifierStart` and `$JavaIdentifierPart` are generated
by running [`UnicodeGen.kt`](/misc/UnicodeGen.kt) with appropriate JDK version.
