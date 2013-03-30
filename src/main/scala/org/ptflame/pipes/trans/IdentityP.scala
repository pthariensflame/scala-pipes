package org.ptflame.pipes
package trans
import scalaz.Monad

final case class IdentityP[P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A](run: P[Uo, Ui, Di, Do, A]) {

  def map[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => B)(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].map[A, B](this.run)(f))

  def ap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: IdentityP[P, Uo1, Ui1, Di1, Do1, (A => B)])(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].ap[A, B](this.run)(f.run))

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => IdentityP[P, Uo1, Ui1, Di1, Do1, B])(implicit P: Proxy[P]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = IdentityP(P.monad[Uo1, Ui1, Di1, Do1].bind[A, B](this.run)(f andThen { _.run }))

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit P: Proxy[P], ev: A <:< IdentityP[P, Uo1, Ui1, Di1, Do1, B]): IdentityP[P, Uo1, Ui1, Di1, Do1, B] = this.flatMap[Uo1, Ui1, Di1, Do1, B](ev)(P)

  def hoistP[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q]): IdentityP[Q, Uo1, Ui1, Di1, Do1, A1] = IdentityP(f(this.run))

}

object IdentityP extends IdentityPInstances

trait IdentityPInstances0 {

  implicit def IdentityPProxy[P[+_, -_, -_, +_, +_]](implicit Px: Proxy[P]): Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#p] = new IdentityPProxy[P] {

    implicit override val P: Proxy[P] = Px

  }

}

trait IdentityPInstances extends IdentityPInstances0 {

  implicit def IdentityPInteract[P[+_, -_, -_, +_, +_]](implicit Px: Interact[P]): Interact[({ type p[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#p] = new IdentityPInteract[P] {

    implicit override val P: Interact[P] = Px

  }

  implicit def IdentityPMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](implicit Px: Proxy[P]): Monad[({ type f[+a] = IdentityP[P, Uo, Ui, Di, Do, a] })#f] = new IdentityPMonad[P, Uo, Ui, Di, Do] {

    implicit override val P: Proxy[P] = Px

  }

  implicit lazy val IdentityPInteractHoist: InteractHoist[IdentityP] = new IdentityPInteractHoist {}

}

private[trans] sealed trait IdentityPMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends Monad[({ type f[+a] = IdentityP[P, Uo, Ui, Di, Do, a] })#f] {

  implicit val P: Proxy[P]

  @inline override def point[A](a: => A): IdentityP[P, Uo, Ui, Di, Do, A] = IdentityP(P.monad[Uo, Ui, Di, Do].point[A](a))

  @inline override def map[A, B](fa: IdentityP[P, Uo, Ui, Di, Do, A])(f: A => B): IdentityP[P, Uo, Ui, Di, Do, B] = fa.map[Uo, Ui, Di, Do, B](f)(P)

  @inline override def ap[A, B](fa: => IdentityP[P, Uo, Ui, Di, Do, A])(f: => IdentityP[P, Uo, Ui, Di, Do, (A => B)]): IdentityP[P, Uo, Ui, Di, Do, B] = fa.ap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def bind[A, B](fa: IdentityP[P, Uo, Ui, Di, Do, A])(f: A => IdentityP[P, Uo, Ui, Di, Do, B]): IdentityP[P, Uo, Ui, Di, Do, B] = fa.flatMap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def join[A](fa: IdentityP[P, Uo, Ui, Di, Do, IdentityP[P, Uo, Ui, Di, Do, A]]): IdentityP[P, Uo, Ui, Di, Do, A] = fa.flatten[Uo, Ui, Di, Do, A](P, implicitly[IdentityP[P, Uo, Ui, Di, Do, A] <:< IdentityP[P, Uo, Ui, Di, Do, A]])

}

private[trans] sealed trait IdentityPProxy[P[+_, -_, -_, +_, +_]] extends Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#p] {

  implicit val P: Proxy[P]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = IdentityP[P, Uo, Ui, Di, Do, a] })#f] = IdentityP.IdentityPMonad[P, Uo, Ui, Di, Do](P)

  @inline override def request[Uo, Ui, Di, Do](uO: => Uo): IdentityP[P, Uo, Ui, Di, Do, Ui] = IdentityP(P.request[Uo, Ui, Di, Do](uO))

  @inline override def respond[Uo, Ui, Di, Do](dO: => Do): IdentityP[P, Uo, Ui, Di, Do, Di] = IdentityP(P.respond[Uo, Ui, Di, Do](dO))

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => IdentityP[P, Uo, Ui, Mu, Md, A], p2: Di => IdentityP[P, Mu, Md, Di, Do, A]): Di => IdentityP[P, Uo, Ui, Di, Do, A] = P.pull[Uo, Ui, Mu, Md, Di, Do, A](p1 andThen { _.run }, p2 andThen { _.run }) andThen { IdentityP(_) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => IdentityP[P, Uo, Ui, Mu, Md, A], p2: Md => IdentityP[P, Mu, Md, Di, Do, A]): Ui => IdentityP[P, Uo, Ui, Di, Do, A] = P.push[Uo, Ui, Mu, Md, Di, Do, A](p1 andThen { _.run }, p2 andThen { _.run }) andThen { IdentityP(_) }

}

private[trans] sealed trait IdentityPInteract[P[+_, -_, -_, +_, +_]] extends Interact[({ type p[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#p] with IdentityPProxy[P] {

  implicit override val P: Interact[P]

  override def requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1: B1 => IdentityP[P, A1, A2, K1, K2, B2], p2: C1 => IdentityP[P, B1, B2, K1, K2, C2]): C1 => IdentityP[P, A1, A2, K1, K2, C2] = P.requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1 andThen { _.run }, p2 andThen { _.run }) andThen { IdentityP(_) }

  def requestBind[A1, A2, K1, K2, B1, B2, C2](p1: B1 => IdentityP[P, A1, A2, K1, K2, B2])(p2: IdentityP[P, B1, B2, K1, K2, C2]): IdentityP[P, A1, A2, K1, K2, C2] = IdentityP(P.requestBind[A1, A2, K1, K2, B1, B2, C2](p1 andThen { _.run })(p2.run))

  override def respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1: A1 => IdentityP[P, K1, K2, B1, B2, A2], p2: B2 => IdentityP[P, K1, K2, C1, C2, B1]): A1 => IdentityP[P, K1, K2, C1, C2, A2] = P.respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1 andThen { _.run }, p2 andThen { _.run }) andThen { IdentityP(_) }

  def respondBind[K1, K2, B1, B2, A2, C1, C2](p1: IdentityP[P, K1, K2, B1, B2, A2])(p2: B2 => IdentityP[P, K1, K2, C1, C2, B1]): IdentityP[P, K1, K2, C1, C2, A2] = IdentityP(P.respondBind[K1, K2, B1, B2, A2, C1, C2](p1.run)(p2 andThen { _.run }))

}

private[trans] sealed trait IdentityPInteractHoist extends InteractHoist[IdentityP] {

  @inline implicit override def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#f] = IdentityP.IdentityPProxy[P](P)

  @inline implicit override def interact[P[+_, -_, -_, +_, +_]](implicit P: Interact[P]): Interact[({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P, uO, uI, dI, dO, a] })#f] = IdentityP.IdentityPInteract[P](P)

  @inline override def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): IdentityP[P, Uo, Ui, Di, Do, A] = IdentityP(p)

  override def hoistP[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P1, P2]): ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P2, uO, uI, dI, dO, a] })#f] = new ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = IdentityP[P2, uO, uI, dI, dO, a] })#f] {

    @inline def apply[Uo, Ui, Di, Do, A](p: IdentityP[P1, Uo, Ui, Di, Do, A]): IdentityP[P2, Uo, Ui, Di, Do, A] = p.hoistP[Uo, Ui, Di, Do, A, P2](f)

  }

}
