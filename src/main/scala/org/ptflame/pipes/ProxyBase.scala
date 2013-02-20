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

  /**
   * Rearranges internals to structually (not just behaviorally) preserve the monad transformer laws.
   *
   * @param M a [[scalaz.Monad]] instance for `M`
   */
  def observe(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): M[ProxyBaseT[Uo, Ui, Di, Do, M, A]] = p match {
      case Wrap(m) => M.bind(m) { go(_) }
      case r@Pure(_) => M.point(r)
      case r@Request(_, f) => M.point(r.copy(next=(x => f(x).observe)))
      case r@Respond(_, f) => M.point(r.copy(next=(x => f(x).observe)))
    }
    Wrap(go(this))
  }

  def map[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => B)(implicit M: Functor[M]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = p match {
      case r@Request(_, fUi) => r.copy(next=((x: Ui1) => go(fUi(x))))
      case r@Respond(_, fDi) => r.copy(next=((x: Di1) => go(fDi(x))))
      case Wrap(m) => Wrap(M.map(m) { go(_) })
      case Pure(r) => Pure(Need(f(r.value)))
    }
    go(this)
  }
  
  def ap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: ProxyBaseT[Uo1, Ui1, Di1, Do1, M, (A => B)])(implicit M: Functor[M]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = {
    def go(p: ProxyBaseT[Uo1, Ui1, Di1, Do1, M, (A => B)]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = p match {
      case r@Request(_, fUi) => r.copy(next=((x: Ui1) => go(fUi(x))))
      case r@Respond(_, fDi) => r.copy(next=((x: Di1) => go(fDi(x))))
      case Wrap(m) => Wrap(M.map(m) { go(_) })
      case Pure(r) => this.map[Uo1, Ui1, Di1, Do1, B](r.value)(M)
    }
    go(f)
  }

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](f: A => ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B])(implicit M: Functor[M]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = {
    def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = p match {
      case r@Request(_, fUi) => r.copy(next=((x: Ui) => go(fUi(x))))
      case r@Respond(_, fDi) => r.copy(next=((x: Di) => go(fDi(x))))
      case Wrap(m) => Wrap(M.map(m) { go(_) })
      case Pure(r) => f(r.value)
    }
    go(this)
  }

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, B](implicit M: Functor[M], ev: A <:< ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B]): ProxyBaseT[Uo1, Ui1, Di1, Do1, M, B] = this.flatMap[Uo1, Ui1, Di1, Do1, B](ev)(M)

  def hoist[Uo1 >: Uo, Ui1 <: Ui, Di1 <: Di, Do1 >: Do, A1 >: A, N[_]](f: NaturalTransformation[M, N])(implicit M: Monad[M]): ProxyBaseT[Uo1, Ui1, Di1, Do1, N, A1] = {
      def go(p: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo1, Ui1, Di1, Do1, N, A1] = p match {
        case r@Request(_, fUi) => r.copy(next=((x: Ui1) => go(fUi(x))))
        case r@Respond(_, fDi) => r.copy(next=((x: Di1) => go(fDi(x))))
        case Wrap(m) => Wrap(f(M.map(m) { go(_) }))
        case Pure(r) => Pure(r)
      }
      go(this.observe(M))
  }

}

private[pipes] final case class Request[Uo, Ui, Di, Do, M[_], A](get: Need[Uo], next: Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Respond[Uo, Ui, Di, Do, M[_], A](get: Need[Do], next: Di => ProxyBaseT[Uo, Ui, Di, Do, M, A]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Wrap[Uo, Ui, Di, Do, M[_], A](get: M[ProxyBaseT[Uo, Ui, Di, Do, M, A]]) extends ProxyBaseT[Uo, Ui, Di, Do, M, A]()

private[pipes] final case class Pure[M[_], A](get: Need[A]) extends ProxyBaseT[Nothing, Any, Any, Nothing, M, A]()

object ProxyBaseT extends ProxyBaseTInstances

private[pipes] trait ProxyBaseTInstances {

  implicit def ProxyBaseTInteract[M[_]](implicit Mx: Functor[M]): Interact[({ type f[+uO, -uI, -dI, +dO, +a] = ProxyBaseT[uO, uI, dI, dO, M, a] })#f] = new ProxyBaseTInteract[M] {

    implicit override val M: Functor[M] = Mx

  }

  implicit def ProxyBaseTMonad[Uo, Ui, Di, Do, M[_]](implicit Mx: Functor[M]): Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Functor[M] = Mx

  }

  implicit def ProxyBaseTHoist[Uo, Ui, Di, Do]: Hoist[({ type f[m[_], +a] = ProxyBaseT[Uo, Ui, Di, Do, m, a] })#f] = new ProxyBaseTHoist[Uo, Ui, Di, Do] {}

}

private[pipes] sealed trait ProxyBaseTInteract[M[_]] extends Interact[({ type f[+uO, -uI, -dI, +dO, +a] = ProxyBaseT[uO, uI, dI, dO, M, a] })#f] {
  
  implicit val M: Functor[M]

  @inline implicit override def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = ProxyBaseT.ProxyBaseTMonad[Uo, Ui, Di, Do, M](M)

  override def request[Uo, Ui, Di, Do](uO: => Uo): ProxyBaseT[Uo, Ui, Di, Do, M, Ui] = Request(Need(uO), (x => Pure(Need(x))))

  override def respond[Uo, Ui, Di, Do](dO: => Do): ProxyBaseT[Uo, Ui, Di, Do, M, Di] = Respond(Need(dO), (x => Pure(Need(x))))

  private[this] def pipeTo[Uo, Ui, Mu, Md, Di, Do, A](p: ProxyBaseT[Uo, Ui, Mu, Md, M, A], f: Md => ProxyBaseT[Mu, Md, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = p match {
    case r@Request(_, fUi) => r.copy(next=(x => pipeTo(fUi(x), f)))
    case Respond(dO, fDi) => pipeFrom(fDi, f(dO.value))
    case Wrap(m) => Wrap(M.map(m) { pipeTo(_, f) })
    case r@Pure(_) => r
  }

  private[this] def pipeFrom[Uo, Ui, Mu, Md, Di, Do, A](f: Mu => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p: ProxyBaseT[Mu, Md, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = p match {
    case Request(uO, fUi) =>pipeTo(f(uO.value), fUi)
    case r@Respond(_, fDi) => r.copy(next=(x => pipeFrom(f, fDi(x))))
    case Wrap(m) => Wrap(M.map(m) { pipeFrom(f, _) })
    case r@Pure(_) => r
  }

  override def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Di => ProxyBaseT[Mu, Md, Di, Do, M, A]): Di => ProxyBaseT[Uo, Ui, Di, Do, M, A] = { x => pipeFrom(p1, p2(x)) }

  override def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => ProxyBaseT[Uo, Ui, Mu, Md, M, A], p2: Md => ProxyBaseT[Mu, Md, Di, Do, M, A]): Ui => ProxyBaseT[Uo, Ui, Di, Do, M, A] = { x => pipeTo(p1(x), p2) }

  def requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1: B1 => ProxyBaseT[A1, A2, K1, K2, M, B2], p2: C1 => ProxyBaseT[B1, B2, K1, K2, M, C2]): C1 => ProxyBaseT[A1, A2, K1, K2, M, C2] = {
    implicit val PM: Monad[({ type f[+a] = ProxyBaseT[A1, A2, K1, K2, M, a] })#f] = ProxyBaseT.ProxyBaseTMonad[A1, A2, K1, K2, M](M)
    def go(p: ProxyBaseT[B1, B2, K1, K2, M, C2]): ProxyBaseT[A1, A2, K1, K2, M, C2] = p match {
      case Request(uO, fUi) => PM.bind(p1(uO.value)) { x => go(fUi(x)) }
      case r@Respond(_, f) => r.copy(next=(x => go(f(x))))
      case Wrap(m) => Wrap(M.map(m) { go(_) })
      case r@Pure(_) => r
    }
    { x => go(p2(x)) }
  }

  def respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1: A1 => ProxyBaseT[K1, K2, B1, B2, M, A2], p2: B2 => ProxyBaseT[K1, K2, C1, C2, M, B1]): A1 => ProxyBaseT[K1, K2, C1, C2, M, A2] = {
    implicit val PM: Monad[({ type f[+a] = ProxyBaseT[K1, K2, C1, C2, M, a] })#f] = ProxyBaseT.ProxyBaseTMonad[K1, K2, C1, C2, M](M)
    def go(p: ProxyBaseT[K1, K2, B1, B2, M, A2]): ProxyBaseT[K1, K2, C1, C2, M, A2] = p match {
      case r@Request(_, f) => r.copy(next=(x => go(f(x))))
      case Respond(dO, fDi) => PM.bind(p2(dO.value)) { x => go(fDi(x)) }
      case Wrap(m) => Wrap(M.map(m) { go(_) })
      case r@Pure(_) => r
    }
    { x => go(p1(x)) }
  }

}

private[pipes] sealed trait ProxyBaseTMonad[Uo, Ui, Di, Do, M[_]] extends Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] {

  implicit val M: Functor[M]

  @inline override def map[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => B): ProxyBaseT[Uo, Ui, Di, Do, M, B] = fa.map[Uo, Ui, Di, Do, B](f)(M)

  @inline override def point[A](a: => A): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Pure(Need(a))

  @inline override def ap[A, B](fa: => ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: => ProxyBaseT[Uo, Ui, Di, Do, M, (A => B)]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = fa.ap[Uo, Ui, Di, Do, B](f)(M)

  @inline override def bind[A, B](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A])(f: A => ProxyBaseT[Uo, Ui, Di, Do, M, B]): ProxyBaseT[Uo, Ui, Di, Do, M, B] = fa.flatMap[Uo, Ui, Di, Do, B](f)(M)

  @inline override def join[A](fa: ProxyBaseT[Uo, Ui, Di, Do, M, ProxyBaseT[Uo, Ui, Di, Do, M, A]]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = fa.flatten[Uo, Ui, Di, Do, A](M, implicitly[ProxyBaseT[Uo, Ui, Di, Do, M, A] <:< ProxyBaseT[Uo, Ui, Di, Do, M, A]])

}

private[pipes] sealed trait ProxyBaseTHoist[Uo, Ui, Di, Do] extends Hoist[({ type f[m[_], +a] = ProxyBaseT[Uo, Ui, Di, Do, m, a] })#f] {

  implicit override def apply[M[_]](implicit Mx: Monad[M]): Monad[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f] = new ProxyBaseTMonad[Uo, Ui, Di, Do, M] {

    implicit override val M: Monad[M] = Mx

  }

  override def hoist[M[_], N[_]](f: NaturalTransformation[M, N])(implicit M: Monad[M]): NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] = new NaturalTransformation[({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, M, a] })#f, ({ type f[+a] = ProxyBaseT[Uo, Ui, Di, Do, N, a] })#f] {

    override def apply[A](fa: ProxyBaseT[Uo, Ui, Di, Do, M, A]): ProxyBaseT[Uo, Ui, Di, Do, N, A] = fa.hoist[Uo, Ui, Di, Do, A, N](f)

  }

  override def liftM[M[_], A](fa: M[A])(implicit M: Monad[M]): ProxyBaseT[Uo, Ui, Di, Do, M, A] = Wrap(M.map[A, ProxyBaseT[Uo, Ui, Di, Do, M, A]](fa) { (a: A) => Pure(Need(a)) })

}
