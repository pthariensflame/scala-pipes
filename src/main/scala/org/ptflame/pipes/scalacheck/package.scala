package org.ptflame.pipe
import scalaz.{Monad, MonadPlus, Equal}
import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazProperties.{monad, monadPlus}

package object scalacheck {

  /**
   * Not `implicit` because of recursion.
   */
  def proxyBaseTArbitrary[Uo, Ui, Di, Do, M[_], A](implicit ArbMP: Arbitrary[M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]], ArbA: Arbitrary[A], ArbUo: Arbitrary[Uo], ArbUiToP: Arbitrary[Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]], ArbUo: Arbitrary[Uo], ArbUiToP: Arbitrary[Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]]): Arbitrary[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = Arbitrary[ProxyBaseT[Uo, Ui, Di, Do, M, A]]()
  
  implicit def proxyBaseArbitrary[Uo, Ui, Di, Do, A](implicit ArbA: Arbitrary[A], ArbUo: Arbitrary[Uo], ArbUo: Arbitrary[Uo]): Arbitrary[ProxyBase[Uo, Ui, Di, Do, A]] = {
    implicit lazy val v: Arbitrary[ProxyBase[Uo, Ui, Di, Do, A]] = proxyBaseTArbitrary[Uo, Ui, Di, Do, Id, A]
    v
  }

}

package scalacheck {

  trait ProxyProperties[P[+_, -_, -_, +_, +_]] { this: Properties =>

    implicit val P: Proxy[P]

    implicit val PM: Monad[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f] = P.monad[Byte, Short, Long, Char]

    implicit val ArbPInt: Arbitrary[P[Byte, Short, Long, Char, Int]]

    implicit val EqPInt: Equal[P[Byte, Short, Long, Char, Int]]

    implicit val ArbPIntToInt: Arbitrary[P[Byte, Short, Long, Char, (Int => Int)]]
    
    property("monad") = monad.laws[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f]

  }

  trait ProxyPlusProperties[P[+_, -_, -_, +_, +_]] extends ProxyProperties[P] { this: Properties =>

    implicit override val P: ProxyPlus[P]

    implicit override val PM: MonadPlus[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f] = P.monad[Byte, Short, Long, Char]

    property("monadPlus") = monadPlus.laws[({ type f[+a] = P[Byte, Short, Long, Char, a] })#f]

  }

  trait InteractProperties[P[+_, -_, -_, +_, +_]] extends ProxyProperties[P] { this: Properties =>

    implicit override val P: Interact[P]

  }

}
