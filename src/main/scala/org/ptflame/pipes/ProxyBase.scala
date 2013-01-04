package org.ptflame.pipes
package internal
import scala.annotation.tailrec, scalaz.Monad

sealed abstract class ProxyBaseT[+Ui, -Uo, -Do, +Di, M[+_], +A] {

  @tailrec def run(implicit M: Monad[M], evU: Uo <:< Unit, evD: Do <:< Unit): M[A] = a

  @tailrec def observe(implicit M: Monad[M]): ProxyBase[Ui, Uo, Do, Di, M, A]

}

private[pipes] final class Request[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: Ui, val go: Uo => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

private[pipes] final class Respond[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: Di, val go: Do => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

private[pipes] final class Wrap[+Ui, -Uo, -Do, +Di, M[+_], +A](val get: M[ProxyBase[Ui, Uo, Do, Di, M, A]]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

private[pipes] final class Pure[M[+_], +A](val get: A) extends ProxyBase[Nothing, Any, Any, Nothing, M, A]

object ProxyBaseT {

}

private[pipes] sealed abstract class ProxyBaseTMonad[Ui, Uo, Do, Di, M[+_]] {

  implicit val M: Functor[M]

}
