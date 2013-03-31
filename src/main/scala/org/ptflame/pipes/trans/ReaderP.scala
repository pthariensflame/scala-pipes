package org.ptflame.pipes
package trans
import scalaz.{Monad, MonadReader}

sealed trait ReaderP[-E, P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A] {

  def run(x: E): P[Uo, Ui, Di, Do, A]

  def map[E1 <: E, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => B)(implicit P: Proxy[P]): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B] = ReaderP { x => P.monad[Uo1, Ui1, Di1, Do1].map[A, B](this.run(x))(f) }

  def ap[E1 <: E, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: ReaderP[E1, P, Uo1, Ui1, Di1, Do1, (A => B)])(implicit P: Proxy[P]): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B] = ReaderP { x => P.monad[Uo1, Ui1, Di1, Do1].ap[A, B](this.run(x))(f.run(x)) }

  def flatMap[E1 <: E, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B])(implicit P: Proxy[P]): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B] = ReaderP { x => P.monad[Uo1, Ui1, Di1, Do1].bind[A, B](this.run(x))(f andThen { _.run(x) }) }

  @inline def flatten[E1 <: E, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit P: Proxy[P], ev: A <:< ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B]): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, B] = this.flatMap[E1, Uo1, Ui1, Di1, Do1, B](ev)(P)

  def hoistP[E1 <: E, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q]): ReaderP[E1, Q, Uo1, Ui1, Di1, Do1, A1] = ReaderP { x => f(this.run(x)) }

  def scope[E1, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A](k: E): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, A1] = ReaderP { _ => this.run(k) }

  def local[E1, Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A](f: E1 => E): ReaderP[E1, P, Uo1, Ui1, Di1, Do1, A1] = ReaderP(f andThen { this.run(_) })

}

object ReaderP extends ReaderPInstances {

  def apply[E, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](f: E => P[Uo, Ui, Di, Do, A]): ReaderP[E, P, Uo, Ui, Di, Do, A] = new ReaderP[E, P, Uo, Ui, Di, Do, A] {

    @inline override def run(x: E): P[Uo, Ui, Di, Do, A] = f(x)

  }

}

sealed trait ReaderPInstances0 {

  implicit def ReaderPProxy[E, P[+_, -_, -_, +_, +_]](implicit Px: Proxy[P]): Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#p] = new ReaderPProxy[E, P] {

    implicit override val P: Proxy[P] = Px

  }

}

trait ReaderPInstances extends ReaderPInstances0 {

  implicit def ReaderPInteract[E, P[+_, -_, -_, +_, +_]](implicit Px: Interact[P]): Interact[({ type p[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#p] = new ReaderPInteract[E, P] {

    implicit override val P: Interact[P] = Px

  }

  implicit def ReaderPMonadReader[E, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](implicit Px: Proxy[P]): MonadReader[({ type f[-e, +a] = ReaderP[e, P, Uo, Ui, Di, Do, a] })#f, E] = new ReaderPMonadReader[E, P, Uo, Ui, Di, Do] {

    implicit override val P: Proxy[P] = Px

  }

  implicit def ReaderPInteractHoist[E]: InteractHoist[({ type p[q[+_, -_, -_, +_, +_], +uO, -uI, -dI, +dO, +a] = ReaderP[E, q, uO, uI, dI, dO, a] })#p] = new ReaderPInteractHoist[E] {}

}

private[trans] sealed trait ReaderPMonadReader[E, P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends MonadReader[({ type f[-e, +a] = ReaderP[e, P, Uo, Ui, Di, Do, a] })#f, E] {

  implicit val P: Proxy[P]

  implicit val PM: Monad[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f] = P.monad[Uo, Ui, Di, Do]

  override def point[A](a: => A): ReaderP[E, P, Uo, Ui, Di, Do, A] = ReaderP { _ => PM.point[A](a) }

  @inline override def map[A, B](fa: ReaderP[E, P, Uo, Ui, Di, Do, A])(f: A => B): ReaderP[E, P, Uo, Ui, Di, Do, B] = fa.map[E, Uo, Ui, Di, Do, B](f)(P)

  @inline override def ap[A, B](fa: => ReaderP[E, P, Uo, Ui, Di, Do, A])(f: => ReaderP[E, P, Uo, Ui, Di, Do, (A => B)]): ReaderP[E, P, Uo, Ui, Di, Do, B] = fa.ap[E, Uo, Ui, Di, Do, B](f)(P)

  @inline override def bind[A, B](fa: ReaderP[E, P, Uo, Ui, Di, Do, A])(f: A => ReaderP[E, P, Uo, Ui, Di, Do, B]): ReaderP[E, P, Uo, Ui, Di, Do, B] = fa.flatMap[E, Uo, Ui, Di, Do, B](f)(P)

  @inline override def join[A](fa: ReaderP[E, P, Uo, Ui, Di, Do, ReaderP[E, P, Uo, Ui, Di, Do, A]]): ReaderP[E, P, Uo, Ui, Di, Do, A] = fa.flatten[E, Uo, Ui, Di, Do, A](P, implicitly[ReaderP[E, P, Uo, Ui, Di, Do, A] <:< ReaderP[E, P, Uo, Ui, Di, Do, A]])

  override def ask: ReaderP[E, P, Uo, Ui, Di, Do, E] = ReaderP { PM.point[E](_) }

  @inline override def local[A](f: E => E)(fa: ReaderP[E, P, Uo, Ui, Di, Do, A]): ReaderP[E, P, Uo, Ui, Di, Do, A] = fa.local[E, Uo, Ui, Di, Do, A](f)

  override def asks[A](f: E => A): ReaderP[E, P, Uo, Ui, Di, Do, A] = ReaderP(f andThen { PM.point[A](_) })

  @inline override def scope[A](k: E)(fa: ReaderP[E, P, Uo, Ui, Di, Do, A]): ReaderP[E, P, Uo, Ui, Di, Do, A] = fa.scope[E, Uo, Ui, Di, Do, A](k)

}

private[trans] sealed trait ReaderPProxy[E, P[+_, -_, -_, +_, +_]] extends Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#p] {

  implicit val P: Proxy[P]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = ReaderP[E, P, Uo, Ui, Di, Do, a] })#f] = ReaderP.ReaderPMonadReader[E, P, Uo, Ui, Di, Do](P)

  @inline override def request[Uo, Ui, Di, Do](uO: => Uo): ReaderP[E, P, Uo, Ui, Di, Do, Ui] = ReaderP { _ => P.request[Uo, Ui, Di, Do](uO) }

  @inline override def respond[Uo, Ui, Di, Do](dO: => Do): ReaderP[E, P, Uo, Ui, Di, Do, Di] = ReaderP { _ => P.respond[Uo, Ui, Di, Do](dO) }

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => ReaderP[E, P, Uo, Ui, Mu, Md, A], p2: Di => ReaderP[E, P, Mu, Md, Di, Do, A]): Di => ReaderP[E, P, Uo, Ui, Di, Do, A] =  { v => ReaderP { x => P.pull[Uo, Ui, Mu, Md, Di, Do, A](p1 andThen { _.run(x) }, p2 andThen { _.run(x) })(v) } }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => ReaderP[E, P, Uo, Ui, Mu, Md, A], p2: Md => ReaderP[E, P, Mu, Md, Di, Do, A]): Ui => ReaderP[E, P, Uo, Ui, Di, Do, A] = { v => ReaderP { x => P.push[Uo, Ui, Mu, Md, Di, Do, A](p1 andThen { _.run(x) }, p2 andThen { _.run(x) })(v) } }

}

private[trans] sealed trait ReaderPInteract[E, P[+_, -_, -_, +_, +_]] extends Interact[({ type p[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#p] with ReaderPProxy[E, P] {

  implicit override val P: Interact[P]

  override def requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1: B1 => ReaderP[E, P, A1, A2, K1, K2, B2], p2: C1 => ReaderP[E, P, B1, B2, K1, K2, C2]): C1 => ReaderP[E, P, A1, A2, K1, K2, C2] = { (v: C1) => ReaderP { (x: E) => P.requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1 andThen { _.run(x) }, p2 andThen { _.run(x) })(v) } }

  def requestBind[A1, A2, K1, K2, B1, B2, C2](p1: B1 => ReaderP[E, P, A1, A2, K1, K2, B2])(p2: ReaderP[E, P, B1, B2, K1, K2, C2]): ReaderP[E, P, A1, A2, K1, K2, C2] = ReaderP { (x: E) => P.requestBind[A1, A2, K1, K2, B1, B2, C2](p1 andThen { _.run(x) })(p2.run(x)) }

  override def respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1: A1 => ReaderP[E, P, K1, K2, B1, B2, A2], p2: B2 => ReaderP[E, P, K1, K2, C1, C2, B1]): A1 => ReaderP[E, P, K1, K2, C1, C2, A2] = { (v: A1) => ReaderP { (x: E) => P.respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1 andThen { _.run(x) }, p2 andThen { _.run(x) })(v) } }

  def respondBind[K1, K2, B1, B2, A2, C1, C2](p1: ReaderP[E, P, K1, K2, B1, B2, A2])(p2: B2 => ReaderP[E, P, K1, K2, C1, C2, B1]): ReaderP[E, P, K1, K2, C1, C2, A2] = ReaderP { (x: E) => P.respondBind[K1, K2, B1, B2, A2, C1, C2](p1.run(x))(p2 andThen { _.run(x) }) }

}

private[trans] sealed trait ReaderPInteractHoist[E] extends InteractHoist[({ type p[q[+_, -_, -_, +_, +_], +uO, -uI, -dI, +dO, +a] = ReaderP[E, q, uO, uI, dI, dO, a] })#p] {

  @inline implicit override def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#f] = ReaderP.ReaderPProxy[E, P](P)

  @inline implicit override def interact[P[+_, -_, -_, +_, +_]](implicit P: Interact[P]): Interact[({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P, uO, uI, dI, dO, a] })#f] = ReaderP.ReaderPInteract[E, P](P)

  @inline override def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): ReaderP[E, P, Uo, Ui, Di, Do, A] = ReaderP { _ => p }

  override def hoistP[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P1, P2]): ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P2, uO, uI, dI, dO, a] })#f] = new ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = ReaderP[E, P2, uO, uI, dI, dO, a] })#f] {

    @inline def apply[Uo, Ui, Di, Do, A](p: ReaderP[E, P1, Uo, Ui, Di, Do, A]): ReaderP[E, P2, Uo, Ui, Di, Do, A] = p.hoistP[E, Uo, Ui, Di, Do, A, P2](f)

  }

}
