package org.ptflame.pipes
import scalaz.{Monad, Hoist}

sealed trait Proxy[P[+_, -_, +_, -_]] {

	implicit def proxy

}

object Proxy {

  private[Proxy]

}