package org.ptflame.pipes
package trans
import scalaz.{Monad, MonadPlus}

final case class OptionP[P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A](run: P[Uo, Ui, Di, Do, Option[A]]) {

  def map[B](f: A => B)(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, B] = OptionP(P.monad[Uo, Ui, Di, Do].map[Option[A], Option[B]](this.run) {
      case None => None
      case Some(v) => Some(f(v))
    })

  def ap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: OptionP[P, Uo1, Ui1, Di1, Do1, (A => B)])(implicit P: Proxy[P]): OptionP[P, Uo1, Ui1, Di1, Do1, B] = {
    val PM = P.monad[Uo1, Ui1, Di1, Do1]
    OptionP(PM.bind[Option[A], Option[B]](this.run) {
        case None => PM.point[Option[B]](None)
        case Some(v) => PM.map(f.run) { g => g map { h => h(v) } }
      })
  }

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => OptionP[P, Uo1, Ui1, Di1, Do1, B])(implicit P: Proxy[P]): OptionP[P, Uo1, Ui1, Di1, Do1, B] = {
    val PM = P.monad[Uo1, Ui1, Di1, Do1]
    OptionP(PM.bind[Option[A], Option[B]](this.run) {
        case None => PM.point[Option[B]](None)
        case Some(v) => f(v).run
      })
  }

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit P: Proxy[P], ev: A <:< OptionP[P, Uo1, Ui1, Di1, Do1, B]): OptionP[P, Uo1, Ui1, Di1, Do1, B] = this.flatMap[Uo1, Ui1, Di1, Do1, B](ev)(P)

  def hoistP[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q]): OptionP[Q, Uo1, Ui1, Di1, Do1, A1] = OptionP(f(this.run))

  def withFilter(f: A => Boolean)(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, A] = OptionP(P.monad[Uo, Ui, Di, Do].map[Option[A], Option[A]](this.run)(v => v.filter(f)))

  @inline def filter(f: A => Boolean)(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, A] = this.withFilter(f)(P)

  def plus[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A](other: OptionP[P, Uo1, Ui1, Di1, Do1, A1])(implicit P: Proxy[P]): OptionP[P, Uo1, Ui1, Di1, Do1, A1] = OptionP(P.monad[Uo1, Ui1, Di1, Do1].lift2[Option[A], Option[A1], Option[A1]]((x, y) => (x, y) match {
        case (v, None) => v
        case (None, v) => v
        case (v, _) => v
      })(this.run, other.run))

}

object OptionP extends OptionPInstances {

  def none[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): OptionP[P, Nothing, Any, Any, Nothing, Nothing] = OptionP(P.monad[Nothing, Any, Any, Nothing].point[Option[Nothing]](None))

  def some[P[+_, -_, -_, +_, +_], A](x: A)(implicit P: Proxy[P]): OptionP[P, Nothing, Any, Any, Nothing, A] = OptionP(P.monad[Nothing, Any, Any, Nothing].point[Option[A]](Some(x)))

}

trait OptionPInstances {

  implicit def OptionPProxy[P[+_, -_, -_, +_, +_]](implicit Px: Proxy[P]): Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = OptionP[P, uO, uI, dI, dO, a] })#p] = new OptionPProxy[P] {

    implicit override val P: Proxy[P] = Px

  }

  implicit def OptionPMonadPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](implicit Px: Proxy[P]): MonadPlus[({ type f[+a] = OptionP[P, Uo, Ui, Di, Do, a] })#f] = new OptionPMonadPlus[P, Uo, Ui, Di, Do] {

    implicit override val P: Proxy[P] = Px

  }

  implicit lazy val OptionPProxyHoist: ProxyHoist[OptionP] = new OptionPProxyHoist {}

}

private[trans] sealed trait OptionPMonadPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends MonadPlus[({ type f[+a] = OptionP[P, Uo, Ui, Di, Do, a] })#f] {

  implicit val P: Proxy[P]

  @inline override def point[A](a: => A): OptionP[P, Uo, Ui, Di, Do, A] = OptionP(P.monad[Uo, Ui, Di, Do].point[Option[A]](Some(a)))

  @inline override def map[A, B](fa: OptionP[P, Uo, Ui, Di, Do, A])(f: A => B): OptionP[P, Uo, Ui, Di, Do, B] = fa.map[B](f)(P)

  @inline override def ap[A, B](fa: => OptionP[P, Uo, Ui, Di, Do, A])(f: => OptionP[P, Uo, Ui, Di, Do, (A => B)]): OptionP[P, Uo, Ui, Di, Do, B] = fa.ap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def bind[A, B](fa: OptionP[P, Uo, Ui, Di, Do, A])(f: A => OptionP[P, Uo, Ui, Di, Do, B]): OptionP[P, Uo, Ui, Di, Do, B] = fa.flatMap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def join[A](fa: OptionP[P, Uo, Ui, Di, Do, OptionP[P, Uo, Ui, Di, Do, A]]): OptionP[P, Uo, Ui, Di, Do, A] = fa.flatten[Uo, Ui, Di, Do, A](P, implicitly[OptionP[P, Uo, Ui, Di, Do, A] <:< OptionP[P, Uo, Ui, Di, Do, A]])

  @inline override def empty[A]: OptionP[P, Uo, Ui, Di, Do, A] = OptionP(P.monad[Uo, Ui, Di, Do].point[Option[A]](None))

  @inline override def plus[A](a: OptionP[P, Uo, Ui, Di, Do, A], b: => OptionP[P, Uo, Ui, Di, Do, A]): OptionP[P, Uo, Ui, Di, Do, A] = a.plus[Uo, Ui, Di, Do, A](b)(P)

  @inline override def filter[A](fa: OptionP[P, Uo, Ui, Di, Do, A])(f: A => Boolean): OptionP[P, Uo, Ui, Di, Do, A] = fa.withFilter(f)(P)

}

private[trans] sealed trait OptionPProxy[P[+_, -_, -_, +_, +_]] extends Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = OptionP[P, uO, uI, dI, dO, a] })#p] {

  implicit val P: Proxy[P]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = OptionP[P, Uo, Ui, Di, Do, a] })#f] = OptionP.OptionPMonadPlus[P, Uo, Ui, Di, Do](P)

  @inline override def request[Uo, Ui, Di, Do](uO: => Uo): OptionP[P, Uo, Ui, Di, Do, Ui] = OptionP(P.monad[Uo, Ui, Di, Do].map(P.request[Uo, Ui, Di, Do](uO)) { Some(_) })

  @inline override def respond[Uo, Ui, Di, Do](dO: => Do): OptionP[P, Uo, Ui, Di, Do, Di] = OptionP(P.monad[Uo, Ui, Di, Do].map(P.respond[Uo, Ui, Di, Do](dO)) { Some(_) })

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => OptionP[P, Uo, Ui, Mu, Md, A], p2: Di => OptionP[P, Mu, Md, Di, Do, A]): Di => OptionP[P, Uo, Ui, Di, Do, A] = { v => OptionP(P.pull({ (x: Mu) => p1(x).run }, { (x: Di) => p2(x).run })(v)) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => OptionP[P, Uo, Ui, Mu, Md, A], p2: Md => OptionP[P, Mu, Md, Di, Do, A]): Ui => OptionP[P, Uo, Ui, Di, Do, A] = { v => OptionP(P.push({ (x: Ui) => p1(x).run }, { (x: Md) => p2(x).run })(v)) }

}

private[trans] sealed trait OptionPProxyHoist extends ProxyHoist[OptionP] {

  @inline implicit override def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = OptionP[P, uO, uI, dI, dO, a] })#f] = OptionP.OptionPProxy[P](P)

  @inline override def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, A] = OptionP(P.monad[Uo, Ui, Di, Do].map[A, Option[A]](p) { Some(_) })

  override def hoistP[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P1, P2]): ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = OptionP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = OptionP[P2, uO, uI, dI, dO, a] })#f] = new ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = OptionP[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = OptionP[P2, uO, uI, dI, dO, a] })#f] {

    @inline def apply[Uo, Ui, Di, Do, A](p: OptionP[P1, Uo, Ui, Di, Do, A]): OptionP[P2, Uo, Ui, Di, Do, A] = p.hoistP[Uo, Ui, Di, Do, A, P2](f)

  }

}