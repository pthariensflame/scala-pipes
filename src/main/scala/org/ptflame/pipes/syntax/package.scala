package org.ptflame.pipes
import language.higherKinds
import scalaz.syntax.Ops

/**
 * Syntax for proxies.
 */s
package object syntax {

  implicit class ProxyKOps[I, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](override val self: I => P[Uo, Ui, Di, Do, A]) extends Ops[I => P[Uo, Ui, Di, Do, A]] {

    @inline def >->[Ei, Eo](other: Ei => P[Di, Do, Ei, Eo, A])(implicit P: Proxy[P], ev: Di <:< I): Ei => P[Uo, Ui, Ei, Eo, A] = P.pull[Uo, Ui, Di, Do, Ei, Eo, A](self compose ev, other)

    @inline def <-<[Ei, Eo](other: Uo => P[Ei, Eo, Uo, Ui, A])(implicit P: Proxy[P], ev: Di <:< I): Di => P[Ei, Eo, Di, Do, A] = P.pull[Ei, Eo, Uo, Ui, Di, Do, A](other, self compose ev)

    @inline def >~>[Ei, Eo](other: Do => P[Di, Do, Ei, Eo, A])(implicit P: Proxy[P], ev: Ui <:< I): Ui => P[Uo, Ui, Ei, Eo, A] = P.push[Uo, Ui, Di, Do, Ei, Eo, A](self compose ev, other)

    @inline def <~<[Ei, Eo](other: Eo => P[Ei, Eo, Uo, Ui, A])(implicit P: Proxy[P], ev: Ui <:< I): Eo => P[Ei, Eo, Di, Do, A] = P.push[Ei, Eo, Uo, Ui, Di, Do, A](other, self compose ev)

    @inline def \>\[Ei, Eo](other: Ei => P[I, A, Di, Do, Eo])(implicit P: Interact[P]): Ei => P[Uo, Ui, Di, Do, Eo] = P.requestWith[Uo, Ui, Di, Do, I, A, Ei, Eo](self, other)

    @inline def /</[Eo, Ei](other: Uo => P[Eo, Ei, Di, Do, Ui])(implicit P: Interact[P]): I => P[Eo, Ei, Di, Do, A] = P.requestWith[Eo, Ei, Di, Do, Uo, Ui, I, A](other, self)

    @inline def />/[Ei, Eo](other: Do => P[Uo, Ui, Ei, Eo, Di])(implicit P: Interact[P]): I => P[Uo, Ui, Ei, Eo, A] = P.respondWith[Uo, Ui, Di, Do, I, A, Ei, Eo](self, other)

    @inline def \<\[Ei, Eo](other: Ei => P[Uo, Ui, A, I, Eo])(implicit P: Interact[P]): Ei => P[Uo, Ui, Di, Do, Eo] = P.respondWith[Uo, Ui, A, I, Ei, Eo, Di, Do](other, self)

    @inline def onK[R](f: P[Uo, Ui, Di, Do, A] => R): I => R = self andThen f

    @inline def runK(implicit ev: Unit <:< I): P[Uo, Ui, Di, Do, A] = self(ev(()))

  }

  implicit class ProxyV[T](override val self: T) extends Ops[T] {

    @inline def request[P[+_, -_, -_, +_, +_], Ui, Di, Do](implicit P: Proxy[P]): P[T, Ui, Di, Do, Ui] = P.request[T, Ui, Di, Do](self)

    @inline def respond[P[+_, -_, -_, +_, +_], Uo, Ui, Di](implicit P: Proxy[P]): P[Uo, Ui, Di, T, Di] = P.respond[Uo, Ui, Di, T](self)

  }

  implicit class ProxyTransV[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](override val self: P[Uo, Ui, Di, Do, A]) extends Ops[P[Uo, Ui, Di, Do, A]] {

    @inline def liftP[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]](implicit PT: ProxyTrans[PT], P: Proxy[P]): PT[P, Uo, Ui, Di, Do, A] = PT.liftP[P, Uo, Ui, Di, Do, A](self)(P)

  }

  implicit class ProxyTransOps[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_], P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](override val self: PT[P, Uo, Ui, Di, Do, A]) extends Ops[PT[P, Uo, Ui, Di, Do, A]] {

    @inline def hoistP[Pm[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Pm])(implicit PT: ProxyHoist[PT]): PT[Pm, Uo, Ui, Di, Do, A] = (PT.hoistP[P, Pm](f))[Uo, Ui, Di, Do, A](self)

  }

}
