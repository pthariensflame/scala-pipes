/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.ptflame.pipes
package trans

final case class IdentityP[P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A](run: P[Uo, Ui, Di, Do, A]) {

  def map[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => B)(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].map[A, B](this.run)(f))

  def ap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: IdentityP[P, Uo1, Ui1, Di1, Do1, (A => B)])(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].ap[A, B](this.run)(f.run))

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => IdentityP[P, Uo1, Ui1, Di1, Do1, B])(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].bind[A, B](this.run)(f andThen { _.run }))

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit P: Proxy[P], ev: A <:< IdentityP[P, Uo1, Ui1, Di1, Do1, B]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = this.flatMap[Uo1, Ui1, Di1, Do1, B](ev)(P)

  def hoistP[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q]): IdentityP[Q, Uo1, Ui1, Di1, Do1, A1] = IdentityP(f(this.run))

}

object IdentityP extends IdentityPInstances

private[trans] trait IdentityPInstances {

  

}
