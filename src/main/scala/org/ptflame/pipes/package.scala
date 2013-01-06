package org.ptflame

/**
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  @inline implicit def ToProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: I => P[Uo, Ui, Di, Do, A]): ProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A] = new ProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p)

}
