package org.ptflame.pipes
import scalaz.syntax.Ops

final class ProxyOps[-I, P[+_, -_, -_, +_, +_], +Uo, -Ui, -Di, +Do, +A](override val self: I => P[Uo, Ui, Di, Do, A]) extends Ops[I => P[Uo, Ui, Di, Do, A]] {

  def >->[Ei, Eo]()(implicit P: Proxy[P], ev: Uo <:< I)

  def <-<[Eo, Ei]()(implicit P: Proxy[P])

  def >~>

  def <~<

  def />/

  def \<\

  def \>\

  def /</

  def 

}
