- [fsm2regex](http://ivanzuzak.info/noam/webapps/fsm2regex/)

Note that the notation is different: `+` is choice.

The site only allows a limited symbols, what we can do is to break symbols
down into disjoint sets and assign one symbol for each set.

```
#states
s0
s1
s2
s3
s4
s5
s6
#initial
s0
#accepting
s6
#alphabet
q
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
```

gives:

```
qqq(z+q(z+qz))*qqq
```

(`q -> "`, `z -> [^"]`)


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

translation: `/ -> a`, `* -> b`, `[^*/] -> c`


```
ab(c+a+bb*c)*bb*a
```