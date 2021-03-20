## A Java language parser

Reference Specification: [Java SE 16 Edition](https://docs.oracle.com/javase/specs/jls/se16/html/index.html)
- [Chapter 3. Lexical Structure](https://docs.oracle.com/javase/specs/jls/se16/html/jls-3.html)

Just want to experience a bit with alex + happy, which may or may not work out.

Project status: Lexical analysis mostly done.

TODOs:

- Java Spec requires that out-of-range literals resulting in compliation errors.
  For now the plan is to do a scan on parsed tokens so we have previous token available
  (we need this bit of info to tell a negative literal)

- Due to current limitation of `Data.Scientific.Scientific` type, the `base10Exponent` can only store `Int`,
  meaning we should probably throw out raw strings that are too long sooner rather than later.

- We currently have two horrible chunks of data in Lexer, namely `$JavaIdentifierStart`
  and `$JavaIdentifierPart`. I wish I can do something better.

- Use Alex without wrapper - this allows us to be more expressive on error messages.

- Parsing.
