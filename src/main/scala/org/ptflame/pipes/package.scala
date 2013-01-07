package org.ptflame
import scalaz.Id.Id

/**
 * A fairly direct port of Gabriel Gonzalez's Haskell `pipes` library to Scala and Scalaz.
 *
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  type ProxyBase[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, Id, A]

  object ProxyBase extends ProxyBaseTInstances

}

package pipes {

  /**
   * Syntax for proxies.
   */
  package object syntax {

    @inline implicit def ToProxyKOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: I => P[Uo, Ui, Di, Do, A]): ProxyKOps[I, P, Uo, Ui, Di, Do, A] = ProxyKOps[I, P, Uo, Ui, Di, Do, A](p)

    @inline implicit def ToProxyV[T](v: T): ProxyV[T] = ProxyV[T](v)

    @inline implicit def ToProxyTransV[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](v: P[Uo, Ui, Di, Do, A]): ProxyTransV[P, Uo, Ui, Di, Do, A] = ProxyTransV[P, Uo, Ui, Di, Do, A](v)

    @inline implicit def ToProxyTransOps[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_], P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: PT[P, Uo, Ui, Di, Do, A]): ProxyTransOps[PT, P, Uo, Ui, Di, Do, A] = ProxyTransOps[PT, P, Uo, Ui, Di, Do, A](p)
  
  }

}
