package org.ptflame.pipes
package internal
import scala.annotation.tailrec, scalaz.{Functor, Monad, MonadPlus}

/**
 * 
 *
 * @tparam Uo upstream output type of the proxy
 * @tparam Ui upstream input type of the proxy
 * @tparam Di downstream input type of the proxy
 * @tparam Do downstream output type of the proxy
 */
sealed abstract class ProxyBaseT[+Uo, -Ui, -Di, +Do, M[+_], +A]() {

  def run(implicit M: Monad[M], evU: Uo <:< Unit, evD: Do <:< Unit): M[A] = {
    @tailrec def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[A] = p match {
      case Request(_, f)
    }
    go(this)
  }

  def observe(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = {
    @tailrec def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = p match {
      case Wrap(m) => M.bind[ProxyBaseT[Uo, Ui, Di, Do, M, A], ProxyBaseT[Uo, Ui, Di, Do, M, A]](m) { go(_) }
      case r@Pure(_) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r)
      case r@Request(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Uo) => f(x).observe)))
      case r@Respond(_, f) => M.point[ProxyBaseT[Uo, Ui, Di, Do, M, A]](r.copy(next=((x: Uo) => f(x).observe)))
    }
    Wrap[Ui, Uo, Do, Di, M, A](go(this))
  }

}

private[pipes] final case class Request[Uo, Ui, Di, Do, M[+_], A](get: Ui, next: Uo => ProxyBaseT[Ui, Uo, Do, Di, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Respond[Uo, Ui, Di, Do, M[+_], A](get: Di, next: Do => ProxyBaseT[Ui, Uo, Do, Di, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Wrap[Uo, Ui, Di, Do, M[+_], A](get: M[ProxyBaseT[Ui, Uo, Do, Di, M, A]]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Pure[M[+_], A](get: A) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]()

object ProxyBaseT {

}

private[pipes] sealed trait ProxyBaseTMonad[Ui, Uo, Do, Di, M[+_]] {

  implicit val M: Functor[M]

}
