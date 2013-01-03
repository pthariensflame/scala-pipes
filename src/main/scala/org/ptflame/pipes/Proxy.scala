package org.ptflame.pipes
import scalaz.{Monad, Hoist}

sealed trait Proxy[P[+_, -_, +_, -_]] {

	implicit def monad

}

object Proxy {

  private[Proxy]

}