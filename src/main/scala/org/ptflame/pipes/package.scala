package org.ptflame
import language.higherKinds
import scalaz.Monad, scalaz.Id.Id

/**
 * A fairly direct port of Gabriel Gonzalez's Haskell `pipes` library to Scala and Scalaz.
 *
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  implicit val hK: higherKinds.type = higherKinds

  type ProxyBase[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, Id, A]

  object ProxyBase extends ProxyBaseTInstances

  @inline def pointP[P[+_, -_, -_, +_, +_], U, D, A](a: => A)(implicit P: Proxy[P]): P[U, D, U, D, A] = P.monad[U, D, U, D].point(a)

  def pointK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): A => P[U, D, U, D, A] = { x => P.monad[U, D, U, D].point(x) }

  def idP[P[+_, -_, -_, +_, +_], U, D, A](a: => U)(implicit P: Proxy[P]): P[U, D, U, D, A] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(x: => U): P[U, D, U, D, A] = PM.bind(PM.bind(P.request(x))(P.respondK)) { go(_) }
    go(a)
  }

  def idK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): U => P[U, D, U, D, A] = { x => idP[P, U, D, A](x) }

  def coidP[P[+_, -_, -_, +_, +_], U, D, A](a: => D)(implicit P: Proxy[P]): P[U, D, U, D, A] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(x: => D): P[U, D, U, D, A] = PM.bind(PM.bind(P.respond(x))(P.requestK)) { go(_) }
    go(a)
  }

  def coidK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): D => P[U, D, U, D, A] = { x => coidP[P, U, D, A](x) }

}
