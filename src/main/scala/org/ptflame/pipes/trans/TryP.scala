package org.ptflame.pipes
package trans
import scala.util.{Try, Success, Failure}
import scalaz.Monad

final case class TryP[P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A](run: P[Uo, Ui, Di, Do, Try[A]]) {

  def toOptionP(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, A] = OptionP[P, Uo, Ui, Di, Do, A](P.monad[Uo, Ui, Di, Do].map[Try[A], Option[A]](this.run) {
      case Failure(_) => None
      case Success(v) => Some(v)
    })

  def map[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => B)(implicit P: Proxy[P]): TryP[P, Uo1, Ui1, Di1, Do1, B] = TryP(P.monad[Uo1, Ui1, Di1, Do1].map[Try[A], Try[B]](this.run) {
      case Failure(ex) => Failure(ex)
      case Success(v) => Success(f(v))
    })

  def ap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: TryP[P, Uo1, Ui1, Di1, Do1, (A => B)])(implicit P: Proxy[P]): TryP[P, Uo1, Ui1, Di1, Do1, B] = {
    val PM = P.monad[Uo1, Ui1, Di1, Do1]
    TryP(PM.bind[Try[A], Try[B]](this.run) {
        case Failure(ex) => PM.point[Try[B]](Failure(ex))
        case Success(v) => PM.map(f.run) { g => g map { h => h(v) } }
      })
  }

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => TryP[P, Uo1, Ui1, Di1, Do1, B])(implicit P: Proxy[P]): TryP[P, Uo1, Ui1, Di1, Do1, B] = {
    val PM = P.monad[Uo1, Ui1, Di1, Do1]
    TryP(PM.bind[Try[A], Try[B]](this.run) {
        case Failure(ex) => PM.point[Try[B]](Failure(ex))
        case Success(v) => f(v).run
      })
  }

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit P: Proxy[P], ev: A <:< TryP[P, Uo1, Ui1, Di1, Do1, B]): TryP[P, Uo1, Ui1, Di1, Do1, B] = this.flatMap[Uo1, Ui1, Di1, Do1, B](ev)(P)

  def hoistP[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q]): TryP[Q, Uo1, Ui1, Di1, Do1, A1] = TryP(f(this.run))

}

object TryP extends TryPInstances {

  def none[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): TryP[P, Nothing, Any, Any, Nothing, Nothing] = TryP(P.monad[Nothing, Any, Any, Nothing].point[Try[Nothing]](Failure()))

  def some[P[+_, -_, -_, +_, +_], A](x: A)(implicit P: Proxy[P]): TryP[P, Nothing, Any, Any, Nothing, A] = TryP(P.monad[Nothing, Any, Any, Nothing].point[Try[A]](Success(x)))

}

trait TryPInstances {

  implicit def TryPProxy[P[+_, -_, -_, +_, +_]](implicit Px: Proxy[P]): Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#p] = new TryPProxy[P] {

    implicit override val P: Proxy[P] = Px

  }

  implicit def TryPMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](implicit Px: Proxy[P]): Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] = new TryPMonad[P, Uo, Ui, Di, Do] {

    implicit override val P: Proxy[P] = Px

  }

  implicit lazy val TryPProxyHoist: ProxyHoist[TryP] = new TryPProxyHoist {}

}

private[trans] sealed trait TryPMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] {

  implicit val P: Proxy[P]

  @inline override def point[A](a: => A): TryP[P, Uo, Ui, Di, Do, A] = TryP(P.monad[Uo, Ui, Di, Do].point[Try[A]](Success(a)))

  @inline override def map[A, B](fa: TryP[P, Uo, Ui, Di, Do, A])(f: A => B): TryP[P, Uo, Ui, Di, Do, B] = fa.map[Uo, Ui, Di, Do, B](f)(P)

  @inline override def ap[A, B](fa: => TryP[P, Uo, Ui, Di, Do, A])(f: => TryP[P, Uo, Ui, Di, Do, (A => B)]): TryP[P, Uo, Ui, Di, Do, B] = fa.ap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def bind[A, B](fa: TryP[P, Uo, Ui, Di, Do, A])(f: A => TryP[P, Uo, Ui, Di, Do, B]): TryP[P, Uo, Ui, Di, Do, B] = fa.flatMap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def join[A](fa: TryP[P, Uo, Ui, Di, Do, TryP[P, Uo, Ui, Di, Do, A]]): TryP[P, Uo, Ui, Di, Do, A] = fa.flatten[Uo, Ui, Di, Do, A](P, implicitly[TryP[P, Uo, Ui, Di, Do, A] <:< TryP[P, Uo, Ui, Di, Do, A]])

}

private[trans] sealed trait TryPProxy[P[+_, -_, -_, +_, +_]] extends Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#p] {

  implicit val P: Proxy[P]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] = TryP.TryPMonad[P, Uo, Ui, Di, Do](P)

  @inline override def request[Uo, Ui, Di, Do](uO: => Uo): TryP[P, Uo, Ui, Di, Do, Ui] = TryP(P.monad[Uo, Ui, Di, Do].map(P.request[Uo, Ui, Di, Do](uO)) { Success(_) })

  @inline override def respond[Uo, Ui, Di, Do](dO: => Do): TryP[P, Uo, Ui, Di, Do, Di] = TryP(P.monad[Uo, Ui, Di, Do].map(P.respond[Uo, Ui, Di, Do](dO)) { Success(_) })

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => TryP[P, Uo, Ui, Mu, Md, A], p2: Di => TryP[P, Mu, Md, Di, Do, A]): Di => TryP[P, Uo, Ui, Di, Do, A] = { v => TryP(P.pull({ (x: Mu) => p1(x).run }, { (x: Di) => p2(x).run })(v)) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => TryP[P, Uo, Ui, Mu, Md, A], p2: Md => TryP[P, Mu, Md, Di, Do, A]): Ui => TryP[P, Uo, Ui, Di, Do, A] = { v => TryP(P.push({ (x: Ui) => p1(x).run }, { (x: Md) => p2(x).run })(v)) }

}

private[trans] sealed trait TryPProxyHoist extends ProxyHoist[TryP] {

  @inline implicit override def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#f] = TryP.TryPProxy[P](P)

  @inline override def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A] = TryP(P.monad[Uo, Ui, Di, Do].map[A, Try[A]](p) { Success(_) })

  override def hoistP[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P1, P2]): ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P2, uO, uI, dI, dO, a] })#f] = new ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P2, uO, uI, dI, dO, a] })#f] {

    @inline def apply[Uo, Ui, Di, Do, A](p: TryP[P1, Uo, Ui, Di, Do, A]): TryP[P2, Uo, Ui, Di, Do, A] = p.hoistP[Uo, Ui, Di, Do, A, P2](f)

  }

}