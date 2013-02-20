package org.ptflame
import language.higherKinds
import scalaz.Monad, scalaz.Id.Id

/**
 * A fairly direct port of Gabriel Gonzalez's Haskell `pipes` library to Scala and Scalaz.
 *
 * @author Alexander Altman
 */
package object pipes extends ProxyBaseTInstances {

  implicit val hK: higherKinds.type = higherKinds

  type ProxyBase[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, Id, A]

  type ProxyBaseP[M[_]] = {
    type p[+Uo, -Ui, -Di, +Do, +A] = ProxyBaseT[Uo, Ui, Di, Do, M, A]
  }

  object ProxyBase extends ProxyBaseTInstances

  type Pipe[+P[+_, -_, -_, +_, +_], -Ui, +Do, +A] = P[Unit, Ui, Unit, Do, A]

  type PipeP[P[+_, -_, -_, +_, +_]] = {
    type p[-Ui, +Do, +A] = Pipe[P, Ui, Do, A]
  }

  type Copipe[+P[+_, -_, -_, +_, +_], +Uo, -Di, +A] = P[Uo, Unit, Di, Unit, A]

  type CopipeP[P[+_, -_, -_, +_, +_]] = {
    type p[+Uo, -Di, +A] = Copipe[P, Uo, Di, A]
  }

  type Producer[+P[+_, -_, -_, +_, +_], +Do, +A] = P[Nothing, Unit, Unit, Do, A]

  type ProducerP[P[+_, -_, -_, +_, +_]] = {
    type p[+Do, +A] = Producer[P, Do, A]
  }

  type Coproducer[+P[+_, -_, -_, +_, +_], +Uo, +A] = P[Uo, Unit, Unit, Nothing, A]

  type CoproducerP[P[+_, -_, -_, +_, +_]] = {
    type p[+Uo, +A] = Coproducer[P, Uo, A]
  }

  type Consumer[+P[+_, -_, -_, +_, +_], -Ui, +A] = P[Unit, Ui, Unit, Nothing, A]

  type ConsumerP[P[+_, -_, -_, +_, +_]] = {
    type p[-Ui, +A] = Consumer[P, Ui, A]
  }

  type Coconsumer[+P[+_, -_, -_, +_, +_], -Di, +A] = P[Nothing, Unit, Di, Unit, A]

  type CoconsumerP[P[+_, -_, -_, +_, +_]] = {
    type p[-Di, +A] = Coconsumer[P, Di, A]
  }

  type Client[+P[+_, -_, -_, +_, +_], +Uo, -Ui, +A] = P[Uo, Ui, Unit, Nothing, A]

  type ClientP[P[+_, -_, -_, +_, +_]] = {
    type p[+Uo, -Ui, +A] = Client[P, Uo, Ui, A]
  }

  type Server[+P[+_, -_, -_, +_, +_], -Di, +Do, +A] = P[Nothing, Unit, Di, Do, A]

  type ServerP[P[+_, -_, -_, +_, +_]] = {
    type p[-Di, +Do, +A] = Server[P, Di, Do, A]
  }

  type Session[+P[+_, -_, -_, +_, +_], +A] = P[Nothing, Unit, Unit, Nothing, A]

  type SessionP[P[+_, -_, -_, +_, +_]] = {
    type p[+A] = Session[P, A]
  }

  type Pipeline[+P[+_, -_, -_, +_, +_], +A] = P[Nothing, Unit, Unit, Nothing, A]

  type PipelineP[P[+_, -_, -_, +_, +_]] = {
    type p[+A] = Pipeline[P, A]
  }

}
