package org.ptflame.pipes
import language.implicitConversions
import scalaz.{Monad, MonadPlus}

trait Proxy[P[+_, -_, -_, +_, +_]] { self =>

  /**
   * Every proxy is a monad.
   *
   * @tparam Uo upstream output type of the proxies that can be used monadically
   * @tparam Ui upstream input type of the proxies that can be used monadically
   * @tparam Di downstream input type of the proxies that can be used monadically
   * @tparam Do downstream output type of the proxies that can be used monadically
   * @return an instance of [[scalaz.Monad]] for proxies with the specified input and output types
   */
  implicit def monad[Uo, Ui, Di, Do]: Monad[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f]

  def request[Uo, Ui, Di, Do](uO: => Uo): P[Uo, Ui, Di, Do, Ui]

  final def requestK[Uo, Ui, Di, Do]: Uo => P[Uo, Ui, Di, Do, Ui] = { self.request(_) }

  def respond[Uo, Ui, Di, Do](dO: => Do): P[Uo, Ui, Di, Do, Di]

  final def respondK[Uo, Ui, Di, Do]: Do => P[Uo, Ui, Di, Do, Di] = { self.respond(_) }

  /**
   * Compose two proxies blocked on a `respond`, generating a new proxy blocked on a `respond`.  Begins from the downstream end and satisfies every `request` with a `respond`.
   *
   * @tparam Uo upstream output type of the resulting proxy
   * @tparam Ui upstream input type of the resulting proxy
   * @tparam Mu intermediate value type flowing upstream between the composed proxies
   * @tparam Md intermediate value type flowing downstream between the composed proxies
   * @tparam Di downstream input type of the resulting proxy
   * @tparam Do downstream output type of the resulting proxy
   * @tparam A final result type of all proxies involved
   * @param p1 upstream proxy to be composed
   * @param p2 downstream proxy to be composed
   * @return the pull-based proxy composition of p1 and p2
   */
  def pull[Uo, Ui, Mu, Md, Di, Do, A](p1: Mu => P[Uo, Ui, Mu, Md, A], p2: Di => P[Mu, Md, Di, Do, A]): Di => P[Uo, Ui, Di, Do, A]

  /**
   * Compose two proxies blocked on a `request`, generating a new proxy blocked
   * on a `request`. Begins from the upstreamstream end and satisfies every
   * `respond` with a `request`.
   *
   * @tparam Uo upstream output type of the resulting proxy
   * @tparam Ui upstream input type of the resulting proxy
   * @tparam Mu intermediate value type flowing upstream between the composed proxies
   * @tparam Md intermediate value type flowing downstream between the composed proxies
   * @tparam Di downstream input type of the resulting proxy
   * @tparam Do downstream output type of the resulting proxy
   * @tparam A final result type of all proxies involved
   * @param p1 upstream proxy to be composed
   * @param p2 downstream proxy to be composed
   * @return the push-based proxy composition of p1 and p2
   */
  def push[Uo, Ui, Mu, Md, Di, Do, A](p1: Ui => P[Uo, Ui, Mu, Md, A], p2: Md => P[Mu, Md, Di, Do, A]): Ui => P[Uo, Ui, Di, Do, A]

}

object Proxy {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[P] = P

  @inline def pointP[P[+_, -_, -_, +_, +_], U, D, A](a: => A)(implicit P: Proxy[P]): P[U, D, U, D, A] = P.monad[U, D, U, D].point(a)

  def pointK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): A => P[U, D, U, D, A] = { x => pointP[P, U, D, A](x)(P) }

  def idP[P[+_, -_, -_, +_, +_], U, D, A](a: => U)(implicit P: Proxy[P]): P[U, D, U, D, A] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(x: => U): P[U, D, U, D, A] = PM.bind(PM.bind(P.request(x))(P.respondK)) { go(_) }
    go(a)
  }

  def idK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): U => P[U, D, U, D, A] = { x => idP[P, U, D, A](x)(P) }

  def coidP[P[+_, -_, -_, +_, +_], U, D, A](a: => D)(implicit P: Proxy[P]): P[U, D, U, D, A] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(x: => D): P[U, D, U, D, A] = PM.bind(PM.bind(P.respond(x))(P.requestK)) { go(_) }
    go(a)
  }

  def coidK[P[+_, -_, -_, +_, +_], U, D, A](implicit P: Proxy[P]): D => P[U, D, U, D, A] = { x => coidP[P, U, D, A](x)(P) }

  def mapDP[P[+_, -_, -_, +_, +_], Ui, Do, X, A](v: => X)(f: Ui => Do)(implicit P: Proxy[P]): P[X, Ui, X, Do, A] = {
    val PM: Monad[({ type f[+a] = P[X, Ui, X, Do, a] })#f] = P.monad[X, Ui, X, Do]
    def go(x: => X): P[X, Ui, X, Do, A] = PM.bind(PM.bind(P.request(x)) { a => P.respond(f(a)) }) { go(_) }
    go(v)
  }

  def mapDK[P[+_, -_, -_, +_, +_], Ui, Do, X, A](f: Ui => Do)(implicit P: Proxy[P]): X => P[X, Ui, X, Do, A] = { x => mapDP[P, Ui, Do, X, A](x)(f)(P) }

  def mapUP[P[+_, -_, -_, +_, +_], Uo, Di, X, A](v: => Di)(g: Di => Uo)(implicit P: Proxy[P]): P[Uo, X, Di, X, A] = {
    val PM: Monad[({ type f[+a] = P[Uo, X, Di, X, a] })#f] = P.monad[Uo, X, Di, X]
    def go(x: => Di): P[Uo, X, Di, X, A] = PM.bind(PM.bind(P.request(g(x)))(P.respondK)) { go(_) }
    go(v)
  }

  def mapUK[P[+_, -_, -_, +_, +_], Uo, Di, X, A](g: Di => Uo)(implicit P: Proxy[P]): Di => P[Uo, X, Di, X, A] = { x => mapUP[P, Uo, Di, X, A](x)(g)(P) }

  def bimapP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](v: => Di)(f: Ui => Do, g: Di => Uo)(implicit P: Proxy[P]): P[Uo, Ui, Di, Do, A] = {
    val PM: Monad[({ type f[+a] = P[Uo, Ui, Di, Do, a] })#f] = P.monad[Uo, Ui, Di, Do]
    def go(x: => Di): P[Uo, Ui, Di, Do, A] = PM.bind(PM.bind(P.request(g(x))) { a => P.respond(f(a)) }) { go(_) }
    go(v)
  }

  def bimapK[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](f: Ui => Do, g: Di => Uo)(implicit P: Proxy[P]): Di => P[Uo, Ui, Di, Do, A] = { x => bimapP[P, Uo, Ui, Di, Do, A](x)(f, g)(P) }

  def takeB[P[+_, -_, -_, +_, +_], U, D](i: Int)(a: => U)(implicit P: Proxy[P]): P[U, D, U, D, U] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(n: Int, x: => U): P[U, D, U, D, U] = if (n <= 0) PM.point[U](x) else PM.bind(PM.bind(P.request(x))(P.respondK)) { x2 => go((n - 1), x2) }
    go(i, a)
  }

  def takeB_[P[+_, -_, -_, +_, +_], U, D](i: Int)(a: => U)(implicit P: Proxy[P]): P[U, D, U, D, Unit] = {
    val PM: Monad[({ type f[+a] = P[U, D, U, D, a] })#f] = P.monad[U, D, U, D]
    def go(n: Int, x: => U): P[U, D, U, D, Unit] = if (n <= 0) PM.point[Unit](()) else PM.bind(PM.bind(P.request(x))(P.respondK)) { x2 => go((n - 1), x2) }
    go(i, a)
  }

}

trait ProxyPlus[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

  implicit override def monad[Ui, Uo, Do, Di]: MonadPlus[({ type f[+a] = P[Ui, Uo, Do, Di, a] })#f]

}

object ProxyPlus {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: ProxyPlus[P]): ProxyPlus[P] = P

}

trait Interact[P[+_, -_, -_, +_, +_]] extends Proxy[P] {

  def requestWith[A1, A2, K1, K2, B1, B2, C1, C2](p1: B1 => P[A1, A2, K1, K2, B2], p2: C1 => P[B1, B2, K1, K2, C2]): C1 => P[A1, A2, K1, K2, C2]

  def respondWith[K1, K2, B1, B2, A1, A2, C1, C2](p1: A1 => P[K1, K2, B1, B2, A2], p2: B2 => P[K1, K2, C1, C2, B1]): A1 => P[K1, K2, C1, C2, A2]

}

object Interact {

  @inline def apply[P[+_, -_, -_, +_, +_]](implicit P: Interact[P]): Interact[P] = P

}

trait ProxyTrans[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]] {

  implicit def proxy[P[+_, -_, -_, +_, +_]](implicit P: Proxy[P]): Proxy[({ type f[+uO, -uI, -dI, +dO, +a] = PT[P, uO, uI, dI, dO, a] })#f]

  def liftP[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](p: P[Uo, Ui, Di, Do, A])(implicit P: Proxy[P]): PT[P, Uo, Ui, Di, Do, A]

}

object ProxyTrans {

  @inline def apply[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]](implicit PT: ProxyTrans[PT]): ProxyTrans[PT] = PT

}

trait ProxyNaturalTransformation[-P1[+_, -_, -_, +_, +_], +P2[+_, -_, -_, +_, +_]] {

  def apply[Uo, Ui, Di, Do, A](p: P1[Uo, Ui, Di, Do, A]): P2[Uo, Ui, Di, Do, A]

}

object ProxyNaturalTransformation {

  implicit def pNatToFunction[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_], Uo, Ui, Di, Do, A](f: ProxyNaturalTransformation[P1, P2]): P1[Uo, Ui, Di, Do, A] => P2[Uo, Ui, Di, Do, A] = { p => f[Uo, Ui, Di, Do, A](p) }

}

trait ProxyHoist[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]] extends ProxyTrans[PT] {

  def hoistP[P1[+_, -_, -_, +_, +_], P2[+_, -_, -_, +_, +_]](f: ProxyNaturalTransformation[P1, P2]): ProxyNaturalTransformation[({ type f[+uO, -uI, -dI, +dO, +a] = PT[P1, uO, uI, dI, dO, a] })#f, ({ type f[+uO, -uI, -dI, +dO, +a] = PT[P2, uO, uI, dI, dO, a] })#f]

}

object ProxyHoist {

  @inline def apply[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]](implicit PT: ProxyHoist[PT]): ProxyHoist[PT] = PT

}

trait InteractTrans[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]] extends ProxyTrans[PT] {

  implicit def interact[P[+_, -_, -_, +_, +_]](implicit P: Interact[P]): Interact[({ type f[+uO, -uI, -dI, +dO, +a] = PT[P, uO, uI, dI, dO, a] })#f]

}

object InteractTrans {

  @inline def apply[PT[_[+_, -_, -_, +_, +_], +_, -_, -_, +_, +_]](implicit PT: InteractTrans[PT]): InteractTrans[PT] = PT

}
