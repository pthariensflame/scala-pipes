package org.ptflame

package object pipes {
  import scalaz.Free

  type ProxyCorrect[+Uo, -Ui, -Di, +Do, +M[+_], +A] = Free[({ type f[+r] = ProxyF[] })#f]

}
