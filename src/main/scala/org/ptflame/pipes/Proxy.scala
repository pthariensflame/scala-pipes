package org.ptflame.pipes
import scalaz.{Monad, MonadPlus, Hoist}

trait Proxy[P[+_, -_, -_, +_, +_]] {

  implicit def monad[Ui, Uo, Do, Di]: Monad[({ type f[+a] = P[Ui, Uo, Do, Di, M, a] })#f]

  implicit def hoist[Ui, Uo, Do, Di]: Hoist[({ type f[+m[+_], +a] = P[Ui, Uo, Do, Di, m, a] })#f]

  def pull[M[+_]]()(implicit M: F[M])

}

object Proxy {

  @inline def apply[P[+_, -_, -_, +_, +_[+_], +_]](implicit P: Proxy[P]): Proxy[P] = P

}

trait ProxyPlus[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

  implicit override def monad[Ui, Uo, Do, Di, M[+_]]: MonadPlus[({ type f[+a] = P[Ui, Uo, Do, Di, M, a] })#f]

}

object ProxyPlus {

  @inline def apply[P[+_, -_, -_, +_, +_], F[_[+_]]](implicit P: ProxyPlus[P]): ProxyPlus[P] = P

}

trait Interact[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

}

object Interact {

  @inline def apply[P[+_, -_, -_, +_, +_[+_], +_], F[_[+_]]](implicit P: Interact[P]): Interact[P] = P

}