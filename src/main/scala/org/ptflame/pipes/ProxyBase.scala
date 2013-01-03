package org.ptflame.pipes
package internal

sealed abstract class ProxyBase[+Ui, -Uo, -Do, +Di, +M[+_], +A]

object ProxyBase {

  private[ProxyBase] final class Request[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: Ui, val go: Uo => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Respond[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: Di, val go: Do => ProxyBase[Ui, Uo, Do, Di, M, A]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Wrap[+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: M[ProxyBase[Ui, Uo, Do, Di, M, A]]) extends ProxyBase[Ui, Uo, Do, Di, M, A]

  private[ProxyBase] final class Pure+Ui, -Uo, -Do, +Di, +M[+_], +A](val get: R) extends ProxyBase[Ui, Uo, Do, Di, M, A]

}
