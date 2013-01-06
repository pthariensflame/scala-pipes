package org.ptflame.pipes
import scalaz.syntax.Ops

final case class ProxyOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](override val self: I => P[Uo, Ui, Di, Do, A]) extends Ops[I => P[Uo, Ui, Di, Do, A]] {

  @inline def >->[Ei, Eo](other: Ei => P[Di, Do, Ei, Eo, A])(implicit P: Proxy[P], ev: Di <:< I): Ei => P[Uo, Ui, Ei, Eo, A] = P.pull[Uo, Ui, Di, Do, Ei, Eo, A](self compose ev, other)

  @inline def <-<[Eo, Ei](other: Uo => P[Eo, Ei, Uo, Ui, A])(implicit P: Proxy[P], ev: Di <:< I): Di => P[Eo, Ei, Di, Do, A] = P.pull[Ei, Eo, Uo, Ui, Di, Do, A](other, self compose ev)

  @inline def >~>[Ei, Eo](other: Do => P[Di, Do, Ei, Eo, A])(implicit P: Proxy[P], ev: Ui <:< I): Ui => P[Uo, Ui, Ei, Eo, A] = P.push[Uo, Ui, Di, Do, Ei, Eo, A](self compose ev, other)

  @inline def <~<[Eo, Ei](other: Ei => P[Eo, Ei, Uo, Ui, A])(implicit P: Proxy[P], ev: Ui <:< I): Ei => P[Eo, Ei, Di, Do, A] = P.push[Ei, Eo, Uo, Ui, Di, Do, A](other, self compose ev)

  @inline def \>\[Ei, Eo](other: Ei => P[I, A, Di, Do, Eo])(implicit P: Interact[P]): Ei => P[Uo, Ui, Di, Do, Eo] = P.requestWith[Uo, Ui, Di, Do, I, A, Ei, Eo](self, other)

  @inline def /</[Eo, Ei](other: Uo => P[Eo, Ei, Di, Do, Ui])(implicit P: Interact[P]): I => P[Eo, Ei, Di, Do, A] = P.requestWith[Eo, Ei, Di, Do, Uo, Ui, I, A](other, self)

  @inline def />/[Ei, Eo](other: Di => P[Uo, Ui, Ei, Eo, Di])(implicit P: Interact[P]): I => P[Uo, Ui, Ei, Eo, A] = P.respondWith[Uo, Ui, Di, Do, I, A, Ei, Eo](self, other)

  @inline def \<\[Ei, Eo](other: Ei => P[Uo, Ui, A, I, Eo])(implicit P: Interact[P]): Ei => P[Uo, Ui, Di, Do, Eo] = P.respondWith[Uo, Ui, A, I, Ei, Eo, Di, Do](other, self)

  @inline def onK[Pm[+_, -_, -_, +_, +_], Uom, Uim, Dim, Dom, Am](f: P[Uo, Ui, Di, Do, A] => Pm[Uom, Uim, Dim, Dom, Am]): I => Pm[Uom, Uim, Dim, Dom, Am] = self andThen f

  @inline def runK(implicit ev: Unit <:< I): P[Uo, Ui, Di, Do, A] = self(ev(()))

}
