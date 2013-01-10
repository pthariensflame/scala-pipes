package org.ptflame.pipes
import language.higherKinds
import scalaz.{Monad, MonadPlus, Equal, Need}, scalaz.Id.Id
import org.scalacheck.{Properties, Arbitrary}, org.scalacheck.Prop.forAll, org.scalacheck.Gen.{sized, oneOf, fail}
import scalaz.scalacheck.ScalazProperties.{monad, monadPlus}, scalaz.scalacheck.ScalazArbitrary.NeedArbitrary

package object scalacheck {

  /**
   * Not `implicit` because of recursion.
   */
  def proxyBaseTArbitrary[Uo, Ui, Di, Do, M[_], A](implicit ArbMP: Arbitrary[M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]], ArbA: Arbitrary[Need[A]], ArbUo: Arbitrary[Need[Uo]], ArbUiToP: Arbitrary[Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]], ArbDo: Arbitrary[Need[Do]], ArbDiToP: Arbitrary[Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]]): Arbitrary[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = Arbitrary[ProxyBaseT[Uo, Ui, Di, Do, M, A]](sized { s =>
      if (s == 0) (ArbA.arbitrary.map { Pure(_) }) else (if (s < 0) fail else (oneOf(
            (for {
                uO <- ArbUo.arbitrary
                fUi <- ArbUiToP.arbitrary
              } yield (Request(uO, fUi))),
            (for {
                dO <- ArbDo.arbitrary
                fDi <- ArbDiToP.arbitrary
              } yield (Respond(dO, fDi))),
            (ArbMP.arbitrary.map { Wrap(_) })
          )))
    })
  
  implicit def proxyBaseArbitrary[Uo, Ui, Di, Do, A](implicit ArbA: Arbitrary[A], ArbUo: Arbitrary[Uo], ArbDo: Arbitrary[Do]): Arbitrary[ProxyBase[Uo, Ui, Di, Do, A]] = {
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
