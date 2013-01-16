package org.ptflame
import language.higherKinds
import scalaz.Id.Id

/**
 * A fairly direct port of Gabriel Gonzalez's Haskell `pipes` library to Scala and Scalaz.
 *
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  implicit val hK: higherKinds.type = higherKinds

  type ProxyBase[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, Id, A]

  object ProxyBase extends ProxyBaseTInstances

}
