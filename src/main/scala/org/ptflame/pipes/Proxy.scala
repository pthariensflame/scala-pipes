package org.ptflame.pipes
import scalaz.{Monad, Hoist}

trait Proxy[P[+_, -_, -_, +_, +_[+_], +_]] {

  implicit def monad[Ui, Uo, Do, Di, M[+_]](implicit M: Monad[M]): Monad[({ type f[+a] = P[Ui, Uo, Do, Di, M, a] })#f]

  implicit def hoist[Ui, Uo, Do, Di]: Hoist[({ type f[+m[+_], +a] = P[Ui, Uo, Do, Di, m, a] })#f]

}

}

object Proxy {

  @inline def apply[P[+_, -_, -_, +_, +_[+_], +_]](implicit P: Proxy[P]): Proxy[P] = P

}