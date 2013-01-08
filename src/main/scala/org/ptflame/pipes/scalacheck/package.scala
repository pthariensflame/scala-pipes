package org.ptflame.pipes
import scalaz.{Monad, MonadPlus}, org.scalacheck.{Properties, Arbitrary, Gen}, org.scalacheck.Prop.forAll, scalaz.scalacheck.ScalazProperties.{monad, monadPlus}

package object scalacheck {

  def proxyProperties[P[+_, -_, -_, +_, +_]](Px: Proxy[P]): Properties = new Properties("proxy") with ProxyProperties[P] {

    implicit override val P: Proxy[P] = Px

  }

  def proxyPlusProperties[P[+_, -_, -_, +_, +_]](Px: ProxyPlus[P]): Properties = new Properties("proxy") with ProxyPlusProperties[P] {

    implicit override val P: ProxyPlus[P] = Px

  }

  implicit def proxyArbitrary[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A] = Arbitrary()

}

package scalacheck {

  private[scalacheck] trait ProxyProperties[P[+_, -_, -_, +_, +_]] { this: Properties =>

    implicit val P: Proxy[P]

    implicit val PM: Monad[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f] = P.monad[Byte, Short, Long, Char]
    
    property("monad") = monad.laws[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f]

  }

  private[scalacheck] trait ProxyPlusProperties[P[+_, -_, -_, +_, +_]] extends ProxyProperties[P] { this: Properties =>

    implicit override val P: ProxyPlus[P]

    implicit override val PM: MonadPlus[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f] = P.monad[Byte, Short, Long, Char]

    property("monadPlus") = monadPlus.laws[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f]

  }

}
