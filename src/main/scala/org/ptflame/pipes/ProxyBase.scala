package org.ptflame.pipes
import scalaz.{Functor, Monad, Hoist, Need, NaturalTransformation}

/**
 * 
 *
 * @tparam Uo upstream output type of the proxy
 * @tparam Ui upstream input type of the proxy
 * @tparam Di downstream input type of the proxy
 * @tparam Do downstream output type of the proxy
 */
sealed abstract class ProxyBaseT[+Uo, -Ui, -Di, +Do, M[_], +A]() {

  def run[A1 >: A](implicit M: Monad[M], evU: Unit <:< Ui, evD: Unit <:< Di): M[A1] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A1]): M[A1] = p match {
      case Request(_, f) => go(f(evU(())))
      case Respond(_, f) => go(f(evD(())))
      case Wrap(m) => M.bind(m) { go(_) }
      case Pure(r) => M.point(r.value)
    }
    go(this)
  }

  def observe(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = p match {
      case Wrap(m) => M.bind(m) { go(_) }
      case r@Pure(_) => M.point(r)
      case r@Request(_, f) => M.point(r.copy(next=(x => f(x).observe)))
      case r@Respond(_, f) => M.point(r.copy(next=(x => f(x).observe)))
    }
    Wrap(go(this))
  }

}

private[pipes] final case class Request[Uo, Ui, Di, Do, M[_], A](get: Need[Uo], next: Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Respond[Uo, Ui, Di, Do, M[_], A](get: Need[Do], next: Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Wrap[Uo, Ui, Di, Do, M[_], A](get: M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Pure[M[_], A](get: Need[A]) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]()

trait ProxyBaseTInstances {

  implicit def ProxyBaseTProxy[M[_]](implicit Mx: Functor[M]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = ProxyBaseT[uO, uI, dI, dO, M, a] })#f] = new ProxyBaseTProxy[M] {

    implicit override val M: Functor[M] = Mx

  }

  implicit def ProxyBaseTMonad[Uo, Ui, Di, Do, M[_]](implicit Mx: Functor[M]): Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Functor[M] = Mx

  }

  implicit def ProxyBaseTHoist[Uo, Ui, Di, Do]: Hoist[({ type f[m[_], +a] = ProxyBaseT[Uo, Ui, Di, Do, m, a] })#f] = new ProxyBaseTHoist[Uo, Ui, Di, Do] {}

}

object ProxyBaseT extends ProxyBaseTInstances

private[pipes] sealed trait ProxyBaseTProxy[M[_]] extends Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = ProxyBaseT[uO, uI, dI, dO, M, a] })#f] { self =>

  implicit val M: Functor[M]

  implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Functor[M] = self.M

  }

  override def request[Uo, Ui, Di, Do](uO: => Uo): ProxyBaseT[Uo, Ui, Di, Do, M, Ui] = Request(Need(uO), (x => Pure(Need(x))))

  override def respond[Uo, Ui, Di, Do](dO: => Do): ProxyBaseT[Uo, Ui, Di, Do, M, Di] = Respond(Need(dO), (x => Pure(Need(x))))

  private[this] def pipeTo[Uo, Ui, Mu, Md, Di, Do, A](p: ProxyBaseT[Uo, Ui, Mu, Md, M, A], f: Md => ProxyBaseT[Mu, Md, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = p match {
    case r@Request(_, fUi) => r.copy(next=(x => self.pipeTo(fUi(x), f)))
    case Respond(dO, fDi) => self.pipeFrom(fDi, f(dO.value))
    case Wrap(m) => Wrap(M.map(m) { self.pipeTo(_, f) })
    case Pure(r) => Pure(r)
  }

  private[this] def pipeFrom[Uo, Ui, Mu, Md, Di, Do, A](f: Mu => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p: ProxyBaseT[Mu, Md, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = p match {
    case Request(uO, fUi) => self.pipeTo(f(uO.value), fUi)
    case r@Respond(_, fDi) => r.copy(next=(x => self.pipeFrom(f, fDi(x))))
    case Wrap(m) => Wrap(M.map(m) { self.pipeFrom(f, _) })
    case Pure(r) => Pure(r)
  }

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Di => ProxyBaseT[Mu, Md, Di, Do, M, A]): Di => ProxyBaseT[Uo, Ui, Di, Do, M, A] = { x => self.pipeFrom(p1, p2(x)) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Md => ProxyBaseT[Mu, Md, Di, Do, M, A]): Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A] = { x => self.pipeTo(p1(x), p2) }

}

private[pipes] sealed trait ProxyBaseTMonad[Uo, Ui, Di, Do, M[_]] extends Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] { self =>

  implicit val M: Functor[M]

  override def map[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => B): ProxyBaseT[Uo, Ui, Di, Do, M, B] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = p match {
        case r@Request(_, fUi) => r.copy(next=((x: Ui) => go(fUi(x))))
        case r@Respond(_, fDi) => r.copy(next=((x: Di) => go(fDi(x))))
        case Wrap(m) => Wrap(M.map(m) { go(_) })
        case Pure(r) => Pure(Need(f(r.value)))
    }
    go(fa)
  }

  @inline override def point[A](a: => A): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Pure(Need(a))

  override def ap[A, B](fa: => ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: => ProxyBaseT[Uo, Ui, Di, Do, M, (A => B)]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, (A => B)]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = p match {
        case r@Request(_, fUi) => r.copy(next=((x: Ui) => go(fUi(x))))
        case r@Respond(_, fDi) => r.copy(next=((x: Di) => go(fDi(x))))
        case Wrap(m) => Wrap(M.map(m) { go(_) })
        case Pure(r) => self.map(fa)(r.value)
    }
    go(f)
  }

  override def bind[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => ProxyBaseT[Uo, Ui, Di, Do, M, B]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = p match {
        case r@Request(_, fUi) => r.copy(next=((x: Ui) => go(fUi(x))))
        case r@Respond(_, fDi) => r.copy(next=((x: Di) => go(fDi(x))))
        case Wrap(m) => Wrap(M.map(m) { go(_) })
        case Pure(r) => f(r.value)
    }
    go(fa)
  }

}

private[pipes] sealed trait ProxyBaseTHoist[Uo, Ui, Di, Do] extends Hoist[({ type f[m[_], +a] = ProxyBaseT[Uo, Ui, Di, Do, m, a] })#f] {

  implicit override def apply[M[_]](implicit Mx: Monad[M]): Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Monad[M] = Mx

  }

  override def hoist[M[_], N[_]](f: NaturalTransformation[M, N])(implicit M: Monad[M]): NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] = new NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] {

    override def apply[A](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, N, A] = {
      def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, N, A] = p match {
        case r@Request(_, fUi) => r.copy(next=((x: Ui) => go(fUi(x))))
        case r@Respond(_, fDi) => r.copy(next=((x: Di) => go(fDi(x))))
        case Wrap(m) => Wrap(f(M.map(m) { go(_) }))
        case Pure(r) => Pure(r)
      }
      go(fa.observe)
    }

  }

  override def liftM[M[_], A](fa: M[A])(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Wrap(M.map[A, ProxyBaseT[Uo, Ui, Di, Do, M, A]](fa) { (a: A) => Pure(Need(a)) })

}
