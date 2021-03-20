# Implementation notes

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
