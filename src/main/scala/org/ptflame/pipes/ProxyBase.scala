package org.ptflame.pipes
package internal
import scala.annotation.tailrec, scalaz.Monad

sealed abstract class ProxyBase[+Ui, -Uo, -Do, +Di, +M[+_], +A] {

  @tailrec def run[M1[X] >: M[X]](implicit M: Monad[M1], evU: Uo <:< Unit, evD: Do <:< Unit): M1[A] = 

  @tailrec def observe[M1[X] >: M[X]](implicit M: Monad[M1]): ProxyBase[Ui, Uo, Do, Di, M1, A]

}

object ProxyBase {

  private[ProxyBase] final class Request[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: Ui, val go: Uo => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Respond[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: Di, val go: Do => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Wrap[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: M[ProxyBase[Ui, Uo, Do, Di, M, A]]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Pure[+M[+_], +A](val get: A) extends ProxyBase[Nothing, Any, Any, Nothing, M, A]

}
