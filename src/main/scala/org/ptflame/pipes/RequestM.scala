package org.ptflame.pipes
import scalaz.{Plus, Monad, PlusEmpty, MonadPlus, Semigroup, Monoid, Foldable, Foldable1}

class RequestM[P[+_, -_, -_, +_, +_], Ui, -Di, +Do, +Uo](val run: P[Uo, Ui, Di, Do, Ui]) /*extends AnyVal*/ {

  def map[Uo1](f: Uo => Uo1)(implicit P: Interact[P]): RequestM[P, Ui, Di, Do, Uo1] = RequestM[P, Ui, Di, Do, Uo1](P.requestBind(f andThen P.requestK[Uo1, Ui, Di, Do])(run))

  def ap[Di1 <: Di, Do1 >: Do, Uo1](f: RequestM[P, Ui, Di1, Do1, (Uo => Uo1)])(implicit P: Interact[P]): RequestM[P, Ui, Di1, Do1, Uo1] = RequestM[P, Ui, Di1, Do1, Uo1](P.requestBind((g: Uo => Uo1) => P.requestBind((x: Uo) => P.request[Uo1, Ui, Di1, Do1](g(x)))(run))(f.run))

  def flatMap[Di1 <: Di, Do1 >: Do, Uo1](f: Uo => RequestM[P, Ui, Di1, Do1, Uo1])(implicit P: Interact[P]): RequestM[P, Ui, Di1, Do1, Uo1] = RequestM[P, Ui, Di1, Do1, Uo1](P.requestBind(f andThen { _.run })(run))

  @inline def flatten[Di1 <: Di, Do1 >: Do, Uo1](implicit P: Interact[P], ev: Uo <:< RequestM[P, Ui, Di1, Do1, Uo1]): RequestM[P, Ui, Di1, Do1, Uo1] = flatMap[Di1, Do1, Uo1](ev)(P)

  def withFilter(f: Uo => Boolean)(implicit P: Interact[P], Ui: Monoid[Ui]): RequestM[P, Ui, Di, Do, Uo] = flatMap[Di, Do, Uo](x => if (f(x)) RequestM.point[P, Ui, Uo](x) else RequestM.empty[P, Ui](P, Ui))(P)

  @inline def filter(f: Uo => Boolean)(implicit P: Interact[P], Ui: Monoid[Ui]): RequestM[P, Ui, Di, Do, Uo] = withFilter(f)(P, Ui)

  def plus[Di1 <: Di, Do1 >: Do, Uo1 >: Uo](other: => RequestM[P, Ui, Di1, Do1, Uo1])(implicit P: Proxy[P], Ui: Semigroup[Ui]): RequestM[P, Ui, Di1, Do1, Uo1] = {
    implicit val PM: Monad[({ type f[+a] = P[Uo1, Ui, Di1, Do1, a] })#f] = P.monad[Uo1, Ui, Di1, Do1]
    RequestM[P, Ui, Di1, Do1, Uo1](PM.lift2[Ui, Ui, Ui]((x: Ui, y: Ui) => Ui.append(x, y))(run, other.run))
  }

}

sealed trait RequestM1 { this: RequestM.type =>

  implicit def RequestMPlus[P[+_, -_, -_, +_, +_], Ui, Di, Do](implicit P0: Proxy[P], Ui0: Semigroup[Ui]): Plus[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] = new RequestMPlus[P, Ui, Di, Do] {
    implicit override val P: Proxy[P] = P0
    implicit override val Ui: Semigroup[Ui] = Ui0
  }

}

sealed trait RequestM0 extends RequestM1 { this: RequestM.type =>

  implicit def RequestMMonad[P[+_, -_, -_, +_, +_], Ui, Di, Do](implicit P0: Interact[P]): Monad[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] = new RequestMMonad[P, Ui, Di, Do] {
    implicit override val P: Interact[P] = P0
  }

  implicit def RequestMPlusEmpty[P[+_, -_, -_, +_, +_], Ui, Di, Do](implicit P0: Proxy[P], Ui0: Monoid[Ui]): PlusEmpty[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] = new RequestMPlusEmpty[P, Ui, Di, Do] {
    implicit override val P: Proxy[P] = P0
    implicit override val Ui: Monoid[Ui] = Ui0
  }

}

object RequestM extends RequestM0 {

  @inline def apply[P[+_, -_, -_, +_, +_], Ui, Di, Do, Uo](v: P[Uo, Ui, Di, Do, Ui]): RequestM[P, Ui, Di, Do, Uo] = new RequestM[P, Ui, Di, Do, Uo](v)

  @inline def unapply[P[+_, -_, -_, +_, +_], Ui, Di, Do, Uo](v: RequestM[P, Ui, Di, Do, Uo]): Some[P[Uo, Ui, Di, Do, Ui]] = Some(v.run)

  @inline def point[P[+_, -_, -_, +_, +_], Ui, Uo](x: => Uo)(implicit P: Proxy[P]): RequestM[P, Ui, Any, Nothing, Uo] = RequestM(P.request(x))

  def pointK[P[+_, -_, -_, +_, +_], Ui, Uo](implicit P: Proxy[P]): Uo => RequestM[P, Ui, Any, Nothing, Uo] = { RequestM.point[P, Ui, Uo](_)(P) }

  @inline def pointAll[P[+_, -_, -_, +_, +_], Ui, F[_], Uo](x: => F[Uo])(implicit P: Proxy[P], Ui: Monoid[Ui], F: Foldable[F]): RequestM[P, Ui, Any, Nothing, Uo] = F.foldMap[Uo, RequestM[P, Ui, Any, Nothing, Uo]](x)(RequestM.pointK[P, Ui, Uo](P))(RequestM.RequestMPlusEmpty[P, Ui, Any, Nothing](P, Ui).monoid[Uo])

  def pointAllK[P[+_, -_, -_, +_, +_], Ui, F[_], Uo](implicit P: Proxy[P], Ui: Monoid[Ui], F: Foldable[F]): F[Uo] => RequestM[P, Ui, Any, Nothing, Uo] = { RequestM.pointAll[P, Ui, F, Uo](_)(P, Ui, F) }

  @inline def pointAll1[P[+_, -_, -_, +_, +_], Ui, F[_], Uo](x: => F[Uo])(implicit P: Proxy[P], Ui: Semigroup[Ui], F: Foldable1[F]): RequestM[P, Ui, Any, Nothing, Uo] = F.foldMap1[Uo, RequestM[P, Ui, Any, Nothing, Uo]](x)(RequestM.pointK[P, Ui, Uo](P))(RequestM.RequestMPlus[P, Ui, Any, Nothing](P, Ui).semigroup[Uo])

  def pointAll1K[P[+_, -_, -_, +_, +_], Ui, F[_], Uo](implicit P: Proxy[P], Ui: Semigroup[Ui], F: Foldable1[F]): F[Uo] => RequestM[P, Ui, Any, Nothing, Uo] = { RequestM.pointAll1[P, Ui, F, Uo](_)(P, Ui, F) }
  
  def empty[P[+_, -_, -_, +_, +_], Ui](implicit P: Proxy[P], Ui: Monoid[Ui]): RequestM[P, Ui, Any, Nothing, Nothing] = {
    implicit val PM: Monad[({ type f[+a] = P[Nothing, Ui, Any, Nothing, a] })#f] = P.monad[Nothing, Ui, Any, Nothing]
    RequestM(PM.point[Ui](Ui.zero))
  }

  implicit def RequestMMonadPlus[P[+_, -_, -_, +_, +_], Ui, Di, Do](implicit P0: Interact[P], Ui0: Monoid[Ui]): MonadPlus[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] = new RequestMMonadPlus[P, Ui, Di, Do] {
    implicit override val P: Interact[P] = P0
    implicit override val Ui: Monoid[Ui] = Ui0
  }

}

private[pipes] sealed trait RequestMMonad[P[+_, -_, -_, +_, +_], Ui, Di, Do] extends Monad[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] {
  implicit val P: Interact[P]

  @inline override def map[A, B](fa: RequestM[P, Ui, Di, Do, A])(f: A => B): RequestM[P, Ui, Di, Do, B] = fa.map[B](f)(P)

  @inline override def bind[A, B](fa: RequestM[P, Ui, Di, Do, A])(f: A => RequestM[P, Ui, Di, Do, B]): RequestM[P, Ui, Di, Do, B] = fa.flatMap[Di, Do, B](f)(P)

  @inline override def point[A](a: => A): RequestM[P, Ui, Di, Do, A] = RequestM.point[P, Ui, A](a)(P)

  @inline override def join[A](ffa: RequestM[P, Ui, Di, Do, RequestM[P, Ui, Di, Do, A]]) = ffa.flatten[Di, Do, A](P, implicitly[RequestM[P, Ui, Di, Do, A] <:< RequestM[P, Ui, Di, Do, A]])

}

private[pipes] sealed trait RequestMPlus[P[+_, -_, -_, +_, +_], Ui, Di, Do] extends Plus[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] {
  implicit val P: Proxy[P]
  implicit val Ui: Semigroup[Ui]

  @inline override def plus[A](a: RequestM[P, Ui, Di, Do, A], b: => RequestM[P, Ui, Di, Do, A]): RequestM[P, Ui, Di, Do, A] = a.plus[Di, Do, A](b)(P, Ui)

}

private[pipes] sealed trait RequestMPlusEmpty[P[+_, -_, -_, +_, +_], Ui, Di, Do] extends PlusEmpty[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] with RequestMPlus[P, Ui, Di, Do] {
  implicit override val Ui: Monoid[Ui]

  @inline override def empty[A]: RequestM[P, Ui, Di, Do, A] = RequestM.empty[P, Ui](P, Ui)

}

private[pipes] sealed trait RequestMMonadPlus[P[+_, -_, -_, +_, +_], Ui, Di, Do] extends MonadPlus[({ type f[+uO] = RequestM[P, Ui, Di, Do, uO] })#f] with RequestMMonad[P, Ui, Di, Do] with RequestMPlusEmpty[P, Ui, Di, Do] {
  implicit override val P: Interact[P]

  @inline override def filter[A](fa: RequestM[P, Ui, Di, Do, A])(f: A => Boolean): RequestM[P, Ui, Di, Do, A] = fa.withFilter(f)(P, Ui)

}
