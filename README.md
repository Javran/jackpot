## A Java language parser

Just want to experience a bit of Alex + Happy, which may or may not work out.

Reference Specification: [Java SE 16 Edition](https://docs.oracle.com/javase/specs/jls/se16/html/index.html)

Chapters of interest:

- [Chapter 3. Lexical Structure](https://docs.oracle.com/javase/specs/jls/se16/html/jls-3.html)
- [Chapter 19. Syntax](https://docs.oracle.com/javase/specs/jls/se16/html/jls-19.html)

Project status: Lexical analysis mostly done.

TODOs:

- Java Spec requires that out-of-range literals resulting in compliation errors.
  We can try to eliminate some of those cases but it's best to do that after we have AST available.
  (as we can't tell whether a minus operator is a binary or unary one before that).

- Due to current limitation of `Data.Scientific.Scientific` type, the `base10Exponent` can only store `Int`,
  meaning we should probably throw out raw strings that are too long sooner rather than later.

- Replace `ReadP` with something that can report finer errors.

- Parsing.

### Non-goals

This project only handles syntactic aspects of Java SE 16.
Compilation errors related to other aspects, for example, type systems are not guaranteed to work.
