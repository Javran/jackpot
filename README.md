## A Java language parser

Reference Specification: [Java SE 16 Edition](https://docs.oracle.com/javase/specs/jls/se16/html/index.html)
- [Chapter 3. Lexical Structure](https://docs.oracle.com/javase/specs/jls/se16/html/jls-3.html)

Just want to experience a bit with alex + happy, which may or may not work out.

Project status: Lexical analysis mostly done.

TODOs:

- Java Spec requires that out-of-range literals resulting in compliation errors.
  We can try to eliminate some of those cases but it's best to do that after we have AST available.
  (as we can't tell whether a minus operator is a binary or unary one before that).

- Due to current limitation of `Data.Scientific.Scientific` type, the `base10Exponent` can only store `Int`,
  meaning we should probably throw out raw strings that are too long sooner rather than later.

- We currently have two horrible chunks of data in Lexer, namely `$JavaIdentifierStart`
  and `$JavaIdentifierPart`. I wish I can do something better.

- Replace `ReadP` with something that can report finer errors.

- Parsing.
