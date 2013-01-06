package org.ptflame.pipes
package internal
import scalaz.{Functor, Monad, Hoist, Need, NaturalTransformation}

/**
 * 
 *
 * @tparam Uo upstream output type of the proxy
 * @tparam Ui upstream input type of the proxy
 * @tparam Di downstream input type of the proxy
 * @tparam Do downstream output type of the proxy
 */
sealed abstract class ProxyBaseT[+Uo, -Ui, -Di, +Do, M[_], +A]() {

  def run(implicit M: Monad[M], evU: Unit <:< Ui, evD: Unit <:< Di): M[A] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[A] = p match {
      case Request(_, f) => go(f(evU(())))
      case Respond(_, f) => go(f(evB(())))
      case Wrap(m) => M.bind[ProxyBaseT[Uo, Ui, Di, Do, M, A], A](m) { go(_) }
      case Pure(r) => M.point[A](r.value)
    }
    go(this)
  }

  def observe(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = p match {
      case Wrap(m) => M.bind[ProxyBaseT[Uo, Ui, Di, Do, M, A], ProxyBaseT[Uo, Ui, Di, Do, M, A]](m) { go(_) }
      case r@Pure(_) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r)
      case r@Request(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Ui) => f(x).observe)))
      case r@Respond(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Di) => f(x).observe)))
    }
    Wrap[Ui, Uo, Do, Di, M, A](go(this))
  }

}

private[pipes] final case class Request[Uo, Ui, Di, Do, M[_], A](get: Need[Uo], next: Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Respond[Uo, Ui, Di, Do, M[_], A](get: Need[Do], next: Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Wrap[Uo, Ui, Di, Do, M[_], A](get: M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Pure[M[_], A](get: Need[A]) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]()

trait ProxyBaseTInstances {

}

object ProxyBaseT extends ProxyBaseTInstances

private[pipes] sealed trait ProxyBaseTProxy[M[_]] extends Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = ProxyBaseT[uO, uI, dI, dO, M, a] })#f] { self =>

  implicit val M: Functor[M]

  implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad {

    implicit override val M: Functor[M] = self.M

  }

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Di => ProxyBaseT[Mu, Md, Di, Do, M, A]): Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Md => ProxyBaseT[Mu, Md, Di, Do, M, A]): Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]

}

private[pipes] sealed trait ProxyBaseTMonad[Uo, Ui, Di, Do, M[_]] extends Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] {

  implicit val M: Functor[M]

  override def map[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => B): ProxyBaseT[Uo, Ui, Di, Do, M, B] = fa match {
    case 
  }

  @inline override def point[A](a: => A): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Pure(Need(a))

  override def ap[A, B](fa: => ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: => ProxyBaseT[Uo, Ui, Di, Do, M, (A => B)]): ProxyBaseT[Uo, Ui, Di, Do, M, B]

  override def bind[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => ProxyBaseT[Uo, Ui, Di, Do, M, B]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = {
    def go()
    go(fa)
  }

}

private[pipes] sealed trait ProxyBaseTHoist[Uo, Ui, Di, Do] extends Hoist[({ type f[m[_], +a] = ProxyBaseT[Uo, Ui, Di, Do, m, a] })#f] {

  implicit override def apply[M[_]](implicit arg0: Monad[M]): Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Monad[M] = arg0

  }

  override def hoist[M[_], N[_]](f: NaturalTransformation[M, N])(implicit arg0: Monad[M]): NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] = new NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] {

    override def apply[A](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, N, A] = {
      def go()
      go(fa.observe)
    }

  }

  override def liftM[M[_], A](fa: M[A])(implicit arg0: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Wrap(arg0.map[A, ProxyBaseT[Uo, Ui, Di, Do, M, A]](fa) { (a: A) => Pure(Need(a)) })

}
