package org.ptflame.pipes
import scalaz.{Functor, Monad, MonadPlus, Equal, Need}
import org.scalacheck.{Properties, Arbitrary, Shrink}, org.scalacheck.Prop.forAll, org.scalacheck.Gen.{sized, oneOf, fail}
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

  /**
   * Not `implicit` because of recursion.
   */
  def proxyBaseTShrink[Uo, Ui, Di, Do, M[_], A](implicit SkMP: Shrink[M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]], SkA: Shrink[A], SkUo: Shrink[Uo], SkDo: Shrink[Do], M: Functor[M]): Shrink[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = Shrink[ProxyBaseT[Uo, Ui, Di, Do, M, A]] {
    case r@Request(o, _) => SkUo.shrink(o.value).map { x => r.copy(get=(Need(x))) }
    case r@Respond(o, _) => SkDo.shrink(o.value).map { x => r.copy(get=(Need(x))) }
    case Wrap(m) => SkMP.shrink(M.map(m) { x => (x: ProxyBaseT[Uo, Ui, Di, Do, M, A]) }).map { Wrap(_) }
    case Pure(r) => SkA.shrink(r.value).map { x => Pure[M, A](Need(x)) }
  }
  
  implicit def proxyBaseArbitrary[Uo, Ui, Di, Do, A](implicit ArbA: Arbitrary[A], ArbUo: Arbitrary[Uo], ArbDo: Arbitrary[Do]): Arbitrary[ProxyBase[Uo, Ui, Di, Do, A]] = {
    implicit lazy val v: Arbitrary[ProxyBase[Uo, Ui, Di, Do, A]] = proxyBaseTArbitrary[Uo, Ui, Di, Do, Id, A]
    v
  }

  implicit def proxyBaseShrink[Uo, Ui, Di, Do, A](implicit SkA: Shrink[A], SkUo: Shrink[Uo], SkDo: Shrink[Do]): Shrink[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = {
    implicit lazy val v: Shrink[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = proxyBaseTShrink[Uo, Ui, Di, Do, Id, A]
    v
  }

}

package scalacheck {

  trait ProxyProperties[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] { this: Properties =>

    implicit val P: Proxy[P]

    implicit val PM: Monad[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f] = P.monad[Uo, Ui, Di, Do]

    implicit def ArbP[A]: Arbitrary[P[Uo, Ui, Di, Do, A]]

    implicit def EqP[A]: Equal[P[Uo, Ui, Di, Do, A]]
    
    include(monad.laws[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f])

  }

  trait ProxyPlusProperties[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends ProxyProperties[P, Uo, Ui, Di, Do] { this: Properties =>

    implicit override val P: ProxyPlus[P]

    implicit override val PM: MonadPlus[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f] = P.monad[Uo, Ui, Di, Do]

    include(monadPlus.laws[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f])

  }

  trait InteractProperties[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends ProxyProperties[P, Uo, Ui, Di, Do] { this: Properties =>

    implicit override val P: Interact[P]

  }

}
