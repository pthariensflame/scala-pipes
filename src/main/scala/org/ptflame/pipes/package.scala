package org.ptflame

/**
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  type ProxyBase[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, Id, A]

  object ProxyBase extends ProxyBaseTInstances

  @inline implicit def ToProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: I => P[Uo, Ui, Di, Do, A]): ProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A] = new ProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p)

}
