package org.ptflame.pipes
package safe
import scala.util.{Try, Success, Failure}
import scalaz.{Monad, Plus}
import org.ptflame.pipes.trans._

final class TryP[P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A] private[TryP] (val run: P[Uo, Ui, Di, Do, Try[A]]) {

  def toOptionP(implicit P: Proxy[P]): OptionP[P, Uo, Ui, Di, Do, A] = OptionP[P, Uo, Ui, Di, Do, A](P.monad[Uo, Ui, Di, Do].map[Try[A], Option[A]](this.run) {
      case Failure(_) => None
      case Success(v) => Some(v)
    })

  def map[B](f: A => B)(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, B] = TryP(P.monad[Uo, Ui, Di, Do].map[Try[A], Try[B]](this.run) {
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

  def hoistP[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, Q[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P, Q])(implicit Q: Proxy[Q]): TryP[Q, Uo1, Ui1, Di1, Do1, A1] = TryP(f(this.run))

  def withFilter(f: A => Boolean)(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A] = TryP(P.monad[Uo, Ui, Di, Do].map[Try[A], Try[A]](this.run)(v => v.filter(f)))

  @inline def filter(f: A => Boolean)(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A] = this.withFilter(f)(P)
  
  def plus[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A](other: TryP[P, Uo1, Ui1, Di1, Do1, A1])(implicit P: Proxy[P]): TryP[P, Uo1, Ui1, Di1, Do1, A1] = TryP(P.monad[Uo1, Ui1, Di1, Do1].lift2[Try[A], Try[A1], Try[A1]]((x, y) => (x, y) match {
        case (v, Failure(_)) => v
        case (Failure(_), v) => v
        case (v, _) => v
      })(this.run, other.run))

  def recover[A1 >: A](f: PartialFunction[Throwable, A1])(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A1] = TryP(P.monad[Uo, Ui, Di, Do].map[Try[A], Try[A1]](this.run) {
      case Failure(ex) => f.lift(ex) match {
          case None => Failure(ex)
          case Some(v) => Success(v)
        }
      case Success(v) => Success(v)
    })

  def recoverWith[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A](f: PartialFunction[Throwable, TryP[P, Uo1, Ui1, Di1, Do1, A1]])(implicit P: Proxy[P]): TryP[P, Uo1, Ui1, Di1, Do1, A1] = {
    implicit val PM: Monad[({ type f[+a] = P[Uo1, Ui1, Di1, Do1, a] })#f] = P.monad[Uo1, Ui1, Di1, Do1]
    TryP(PM.bind[Try[A], Try[A1]](this.run) {
        case Failure(ex) => f.lift(ex) match {
            case None => PM.point[Try[A1]](Failure(ex))
            case Some(a) => a.run
        }
        case Success(v) => PM.point[Try[A1]](Success(v))
      })
  }

}

object TryP extends TryPInstances {

  def failure[P[+_, -_, -_, +_, +_]](ex: Throwable)(implicit P: Proxy[P]): TryP[P, Nothing, Any, Any, Nothing, Nothing] = TryP(P.monad[Nothing, Any, Any, Nothing].point[Try[Nothing]](Failure(ex)))

  def success[P[+_, -_, -_, +_, +_], A](x: A)(implicit P: Proxy[P]): TryP[P, Nothing, Any, Any, Nothing, A] = TryP(P.monad[Nothing, Any, Any, Nothing].point[Try[A]](Success(x)))

  def apply[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](e: => P[Uo, Ui, Di, Do, Try[A]])(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A] = {
    implicit val PM: Monad[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f] = P.monad[Uo, Ui, Di, Do]
    new TryP(Try(e) match {
        case Failure(ex) => PM.point[Try[A]](Failure(ex))
        case Success(v) => v
      })
  }

  def unapply[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](a: TryP[P, Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): Some[P[Uo, Ui, Di, Do, Try[A]]] = Some(a.run)

}

trait TryPInstances {

  implicit def TryPProxy[P[+_, -_, -_, +_, +_]](implicit Px: Proxy[P]): Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#p] = new TryPProxy[P] {
    implicit override val P: Proxy[P] = Px
  }

  implicit def TryPMonadPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](implicit Px: Proxy[P]): Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] with Plus[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] = new TryPMonadPlus[P, Uo, Ui, Di, Do] {
    implicit override val P: Proxy[P] = Px
  }

  implicit lazy val TryPProxyTrans: ProxyTrans[TryP] = new TryPProxyTrans {}

}

private[safe] sealed trait TryPMonadPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do] extends Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] with Plus[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] {
  implicit val P: Proxy[P]

  @inline override def point[A](a: => A): TryP[P, Uo, Ui, Di, Do, A] = TryP(P.monad[Uo, Ui, Di, Do].point[Try[A]](Success(a)))

  @inline override def map[A, B](fa: TryP[P, Uo, Ui, Di, Do, A])(f: A => B): TryP[P, Uo, Ui, Di, Do, B] = fa.map[B](f)(P)

  @inline override def ap[A, B](fa: => TryP[P, Uo, Ui, Di, Do, A])(f: => TryP[P, Uo, Ui, Di, Do, (A => B)]): TryP[P, Uo, Ui, Di, Do, B] = fa.ap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def bind[A, B](fa: TryP[P, Uo, Ui, Di, Do, A])(f: A => TryP[P, Uo, Ui, Di, Do, B]): TryP[P, Uo, Ui, Di, Do, B] = fa.flatMap[Uo, Ui, Di, Do, B](f)(P)

  @inline override def join[A](fa: TryP[P, Uo, Ui, Di, Do, TryP[P, Uo, Ui, Di, Do, A]]): TryP[P, Uo, Ui, Di, Do, A] = fa.flatten[Uo, Ui, Di, Do, A](P, implicitly[TryP[P, Uo, Ui, Di, Do, A] <:< TryP[P, Uo, Ui, Di, Do, A]])

  @inline override def plus[A](a: TryP[P, Uo, Ui, Di, Do, A], b: => TryP[P, Uo, Ui, Di, Do, A]): TryP[P, Uo, Ui, Di, Do, A] = a.plus[Uo, Ui, Di, Do, A](b)(P)

}

private[safe] sealed trait TryPProxy[P[+_, -_, -_, +_, +_]] extends Proxy[({ type p[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#p] {
  implicit val P: Proxy[P]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = TryP[P, Uo, Ui, Di, Do, a] })#f] = TryP.TryPMonadPlus[P, Uo, Ui, Di, Do](P)

  @inline override def request[Uo, Ui, Di, Do](uO: => Uo): TryP[P, Uo, Ui, Di, Do, Ui] = TryP(P.monad[Uo, Ui, Di, Do].map(P.request[Uo, Ui, Di, Do](uO)) { Success(_) })

  @inline override def respond[Uo, Ui, Di, Do](dO: => Do): TryP[P, Uo, Ui, Di, Do, Di] = TryP(P.monad[Uo, Ui, Di, Do].map(P.respond[Uo, Ui, Di, Do](dO)) { Success(_) })

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => TryP[P, Uo, Ui, Mu, Md, A], p2: Di => TryP[P, Mu, Md, Di, Do, A]): Di => TryP[P, Uo, Ui, Di, Do, A] = { v => TryP(P.pull({ (x: Mu) => p1(x).run }, { (x: Di) => p2(x).run })(v)) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => TryP[P, Uo, Ui, Mu, Md, A], p2: Md => TryP[P, Mu, Md, Di, Do, A]): Ui => TryP[P, Uo, Ui, Di, Do, A] = { v => TryP(P.push({ (x: Ui) => p1(x).run }, { (x: Md) => p2(x).run })(v)) }

}

private[safe] sealed trait TryPProxyTrans extends ProxyTrans[TryP] {

  @inline implicit override def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = TryP[P, uO, uI, dI, dO, a] })#f] = TryP.TryPProxy[P](P)

  @inline override def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): TryP[P, Uo, Ui, Di, Do, A] = TryP(P.monad[Uo, Ui, Di, Do].map[A, Try[A]](p) { Success(_) })

}
