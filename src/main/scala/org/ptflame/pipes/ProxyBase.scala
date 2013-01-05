package org.ptflame.pipes
package internal
import scala.annotation.tailrec, scalaz.{Functor, Monad, MonadPlus}

sealed abstract class ProxyBaseT[+Uo, -Ui, -Di, +Do, M[+_], +A] {

  @tailrec def run(implicit M: Monad[M], evU: Uo <:< Unit, evD: Do <:< Unit): M[A] = {
    @tailrec def go()
    go(this)
  }

  def observe(implicit M: Monad[M]): ProxyBaseT[Ui, Uo, Do, Di, M, A] = {
    @tailrec def go(p: ProxyBaseT[Ui, Uo, Do, Di, M, A]): M[ProxyBaseT[Ui, Uo, Do, Di, M, A]] = p match {
      case Wrap(m) => M.bind[ProxyBaseT[Ui, Uo, Do, Di, M, A], ProxyBaseT[Ui, Uo, Do, Di, M, A]](m, go(_))
      case r@Pure(_) => M.point[ProxyBaseT[Ui, Uo, Do, Di, M, A]](r)
      case Request(ui, fUo) =>
      case Respond(di, fDo) => 
    }
    Wrap[Ui, Uo, Do, Di, M, A](go(this))
  }

}

private[pipes] final class Request[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: Ui, val next: Uo => ProxyBaseT[Ui, Uo, Do, Di, M, A]) extends ProxyBaseT[Ui, Uo, Do, Di, M, A]

private[pipes] final class Respond[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: Di, val next: Do => ProxyBaseT[Ui, Uo, Do, Di, M, A]) extends ProxyBaseT[Ui, Uo, Do, Di, M, A]

private[pipes] final class Wrap[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: M[ProxyBaseT[Ui, Uo, Do, Di, M, A]]) extends ProxyBaseT[Ui, Uo, Do, Di, M, A]

private[pipes] final class Pure[M[+_], +A](val get: A) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]

object ProxyBaseT {

}

private[pipes] sealed trait ProxyBaseTMonad[Ui, Uo, Do, Di, M[+_]] {

  implicit val M: Functor[M]

}
