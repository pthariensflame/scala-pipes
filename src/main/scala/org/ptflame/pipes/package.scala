package org.ptflame

/**
 * @author Alexander Altman
 */
package object pipes {
  import scalaz.Id._

  type ProxyBase[+Ui, -Uo, -Do, +Di, +A] = ProxyBaseT[Ui, Uo, Do, Di, Id, A]

}

package pipes {

  

}
