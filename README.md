scala-pipes
===

*scala-pipes* is an in-progress port to Scala and Scalaz of Gabriel Gonzalez's [*pipes*](http://hackage.haskell.org/package/pipes) and [*pipes-safe*](http://hackage.haskell.org/package/pipes-safe) libraries for Haskell.

The design of *scala-pipes* revolves around a central, polymorphic abstraction:  `Proxy`, implemented as a type class.  All instances of `Proxy` can be treated as a kind of bidirectional enumerator, iteratee, or enumeratee.  There is only one type (per `Proxy` transformer), similar to an enumeratee, and enumerator- and iteratee-equivalents are simply type synonyms over the type in question, themselves inherently polymorphic over the underlying `Proxy`.

There is one base `Proxy`, `ProxyBaseT`, which is a `Monad` transformer in addition to being a `Proxy`.  Layerable on top of any other `Proxy` are various `Proxy` transformers, including:

 - `IdentityP`
 - `ReaderP`
