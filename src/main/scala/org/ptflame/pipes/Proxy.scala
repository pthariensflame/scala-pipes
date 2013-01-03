package org.ptflame.pipes

sealed trait Proxy[P[+_, -_, +_, -_]] {

	implicit def proxy

}

object Proxy {

  private[Proxy]

}