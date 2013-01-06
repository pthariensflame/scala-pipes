package org.ptflame.pipes
package internal
import scala.annotation.tailrec, scalaz.{Functor, Monad, MonadPlus}, scalaz.Id.Id

/**
 * 
 *
 * @tparam Uo upstream output type of the proxy
 * @tparam Ui upstream input type of the proxy
 * @tparam Di downstream input type of the proxy
 * @tparam Do downstream output type of the proxy
 */
sealed abstract class ProxyBaseT[+Uo, -Ui, -Di, +Do, M[+_], +A]() {

  def run(implicit M: Monad[M], evU: Unit <:< Ui, evD: Unit <:< Di): M[A] = {
    @tailrec def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[A] = p match {
      case Request(_, f) => go(f(evU(())))
      case Respond(_, f) => go(f(evB(())))
      case Wrap(m) => M.bind[ProxyBaseT[Uo, Ui, Di, Do, M, A], A](m) { go(_) }
      case Pure(r) => M.point[A](r)
    }
    go(this)
  }

  def observe(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = {
    @tailrec def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = p match {
      case Wrap(m) => M.bind[ProxyBaseT[Uo, Ui, Di, Do, M, A], ProxyBaseT[Uo, Ui, Di, Do, M, A]](m) { go(_) }
      case r@Pure(_) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r)
      case r@Request(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Ui) => f(x).observe)))
      case r@Respond(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Di) => f(x).observe)))
    }
    Wrap[Ui, Uo, Do, Di, M, A](go(this))
  }

}

private[pipes] final case class Request[Uo, Ui, Di, Do, M[+_], A](get: Uo, next: Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Respond[Uo, Ui, Di, Do, M[+_], A](get: Do, next: Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Wrap[Uo, Ui, Di, Do, M[+_], A](get: M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Pure[M[+_], A](get: A) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]()

trait ProxyBaseTInstances0 {



}

trait ProxyBaseTInstances extends ProxyBaseTInstances0 {

  type ProxyBase[+Ui, -Uo, -Do, +Di, +A] = ProxyBaseT[Ui, Uo, Do, Di, Id, A]

  type ProxyBaseP[M[+_]] = {
    type P[+Ui, -Uo, -Do, +Di, +A] = ProxyBaseT[Uo, Ui, Di, Do, M, A]
  }

  type ProxyBaseM[+Uo, -Ui, -Di, +Do] = {
    type M[F[+_], A] = ProxyBaseT[Uo, Ui]
  }

}

object ProxyBaseT extends ProxyBaseTInstances

private[pipes] sealed trait ProxyBaseTProxy[M[+_]] extends Proxy[ProxyBaseP[M]#P] {

  implicit val M: Functor[M]

  implicit def

}
