package org.ptflame.pipes
import scalaz.{Monad, MonadPlus}

trait Proxy[P[+_, -_, -_, +_, +_]] {

  implicit def monad[Ui, Uo, Do, Di]: Monad[({ type f[+a] = P[Ui, Uo, Do, Di, a] })#f]

  def pull[Ui, Uo, M1, M2, Do, Di, A](p1: M1 => P[Ui, Uo, M1, M2, A], p2: Do => P[M1, M2, Do, Di, A]): Do => P[Ui, Uo, Do, Di, A]

  def push[Ui, Uo, M1, M2, Do, Di, A](p1: )

}

object Proxy {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[P] = P

}

trait ProxyPlus[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

  implicit override def monad[Ui, Uo, Do, Di]: MonadPlus[({ type f[+a] = P[Ui, Uo, Do, Di, a] })#f]

}

object ProxyPlus {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: ProxyPlus[P]): ProxyPlus[P] = P

}

trait Interact[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

}

object Interact {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: Interact[P]): Interact[P] = P

}