package org.ptflame.pipes
import scalaz.{Plus, Monad, PlusEmpty, MonadPlus, Semigroup, Monoid, Foldable, Foldable1}

class RespondM[P[+_, -_, -_, +_, +_], +Uo, -Ui, Di, +Do](val run: P[Uo, Ui, Di, Do, Di]) /*extends AnyVal*/ {

  def map[Do1](f: Do => Do1)(implicit P: Interact[P]): RespondM[P, Uo, Ui, Di, Do1] = RespondM[P, Uo, Ui, Di, Do1](P.respondBind(run)(f andThen P.respondK[Uo, Ui, Di, Do1]))

  def ap[Uo1 >: Uo, Ui1 <: Ui, Do1](f: RespondM[P, Uo1, Ui1, Di, (Do => Do1)])(implicit P: Interact[P]): RespondM[P, Uo1, Ui1, Di, Do1] = RespondM[P, Uo1, Ui1, Di, Do1](P.respondBind(f.run)((g: Do => Do1) => P.respondBind(run)((x: Do) => P.respond[Uo1, Ui1, Di, Do1](g(x)))))

  def flatMap[Uo1 >: Uo, Ui1 <: Ui, Do1](f: Do => RespondM[P, Di, Ui1, Uo1, Di, Do1])(implicit P: Interact[P]): RespondM[P, Ui1, Uo1, Do1] = RespondM[P, Ui1, Uo1, Di, Do1](P.respondBind(run)(f andThen { _.run }))

  @inline def flatten[Uo1 >: Uo, Ui1 <: Ui, Do1](implicit P: Interact[P], ev: Do <:< RespondM[P, Uo1, Ui1, Di, Do1]): RespondM[P, Uo1, Ui1, Di, Do1] = flatMap[Uo1, Ui1, Do1](ev)(P)

  def withFilter(f: Do => Boolean)(implicit P: Interact[P], Di: Monoid[Di]): RespondM[P, Uo, Ui, Di, Do] = flatMap[Uo, Ui, Do](x => if (f(x)) RespondM.point[P, Di, Do](x) else RespondM.empty[P, Di](P, Di))(P)

  @inline def filter(f: Do => Boolean)(implicit P: Interact[P], Di: Monoid[Di]): RespondM[P, Uo, Ui, Di, Do] = withFilter(f)(P, Di)

  def plus[Uo1 >: Uo, Ui1 <: Ui, Do1 >: Do](other: => RespondM[P, Uo1, Ui1, Di, Do1])(implicit P: Proxy[P], Di: Semigroup[Di]): RespondM[P, Uo1, Ui1, Di, Do1] = {
    implicit val PM: Monad[({ type f[+a] = P[Uo1, Ui1, Di, Do1, a] })#f] = P.monad[Uo1, Ui1, Di, Do1]
    RespondM[P, Uo1, Ui1, Di, Do1](PM.lift2[Di, Di, Di]((x: Di, y: Di) => Di.append(x, y))(run, other.run))
  }

}

sealed trait RespondM1 { this: RespondM.type =>

  implicit def RespondMPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di](implicit P0: Proxy[P], Di0: Semigroup[Di]): Plus[({ type f[+dO] = RespondM[P, Uo, Ui, Di, dO] })#f] = new RespondMPlus[P, Uo, Ui, Di] {
    implicit override val P: Proxy[P] = P0
    implicit override val Di: Semigroup[Di] = Di0
  }

}

sealed trait RespondM0 extends RespondM1 { this: RespondM.type =>

  implicit def RespondMMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di](implicit P0: Interact[P]): Monad[({ type f[+dO] = RespondM[P, Uo, Ui, Di, dO] })#f] = new RespondMMonad[P, Uo, Ui, Di] {
    implicit override val P: Interact[P] = P0
  }

  implicit def RespondMPlusEmpty[P[+_, -_, -_, +_, +_], Uo, Ui, Di](implicit P0: Proxy[P], Di0: Monoid[Di]): PlusEmpty[({ type f[+dO] = RespondM[P, Uo, Ui, Di, dO] })#f] = new RespondMPlusEmpty[P, Uo, Ui, Di] {
    implicit override val P: Proxy[P] = P0
    implicit override val Di: Monoid[Di] = Di0
  }

}

object RespondM extends RespondM0 {

  @inline def apply[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](v: P[Uo, Ui, Di, Do, Di]): RespondM[P, Uo, Ui, Di, Do] = new RespondM[P, Uo, Ui, Di, Do](v)

  @inline def unapply[P[+_, -_, -_, +_, +_], Uo, Ui, Di, Do](v: RespondM[P, Uo, Ui, Di, Do]): Some[P[Uo, Ui, Di, Do, Di]] = Some(v.run)

  @inline def point[P[+_, -_, -_, +_, +_], Di, Do](x: => Do)(implicit P: Proxy[P]): RespondM[P, Nothing, Any, Di, Do] = RespondM(P.respond(x))

  def pointK[P[+_, -_, -_, +_, +_], Di, Do](implicit P: Proxy[P]): Do => RespondM[P, Nothing, Any, Di, Do] = { RespondM.point[P, Di, Do](_)(P) }

  @inline def pointAll[P[+_, -_, -_, +_, +_], Di, F[_], Do](x: => F[Do])(implicit P: Proxy[P], Di: Monoid[Di], F: Foldable[F]): RespondM[P, Nothing, Any, Di, Do] = F.foldMap[Do, RespondM[P, Nothing, Any, Di, Do]](x)(RespondM.pointK[P, Di, Do](P))(RespondM.RespondMPlusEmpty[P, Nothing, Any, Di](P, Di).monoid[Do])

  def pointAllK[P[+_, -_, -_, +_, +_], Di, F[_], Do](implicit P: Proxy[P], Di: Monoid[Di], F: Foldable[F]): F[Do] => RespondM[P, Di, Any, Nothing, Do] = { RespondM.pointAll[P, Di, F, Do](_)(P, Di, F) }

  @inline def pointAll1[P[+_, -_, -_, +_, +_], Di, F[_], Do](x: => F[Do])(implicit P: Proxy[P], Di: Semigroup[Di], F: Foldable1[F]): RespondM[P, Nothing, Any, Di, Do] = F.foldMap1[Do, RespondM[P, Nothing, Any, Di, Do]](x)(RespondM.pointK[P, Di, Do](P))(RespondM.RespondMPlus[P, Nothing, Any, Di](P, Di).semigroup[Do])

  def pointAll1K[P[+_, -_, -_, +_, +_], Di, F[_], Do](implicit P: Proxy[P], Di: Semigroup[Di], F: Foldable1[F]): F[Do] => RespondM[P, Nothing, Any, Di, Do] = { RespondM.pointAll1[P, Di, F, Do](_)(P, Di, F) }
  
  def empty[P[+_, -_, -_, +_, +_], Di](implicit P: Proxy[P], Di: Monoid[Di]): RespondM[P, Nothing, Any, Di, Nothing] = {
    implicit val PM: Monad[({ type f[+a] = P[Nothing, Any, Di, Nothing, a] })#f] = P.monad[Nothing, Any, Di, Nothing]
    RespondM[Nothing, Any, Di, Nothing](PM.point[Di](Di.zero))
  }

  implicit def RespondMMonadPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di](implicit P0: Interact[P], Di0: Monoid[Di]): MonadPlus[({ type f[+uO] = RespondM[P, Di, Ui, Uo, uO] })#f] = new RespondMMonadPlus[P, Di, Ui, Uo] {
    implicit override val P: Interact[P] = P0
    implicit override val Di: Monoid[Di] = Di0
  }

}

private[pipes] sealed trait RespondMMonad[P[+_, -_, -_, +_, +_], Uo, Ui, Di] extends Monad[({ type f[+dO] = RespondM[P, Uo, Ui, Di, dO] })#f] {
  implicit val P: Interact[P]

  @inline override def map[A, B](fa: RespondM[P, Di, Ui, Uo, A])(f: A => B): RespondM[P, Di, Ui, Uo, B] = fa.map[B](f)(P)

  @inline override def bind[A, B](fa: RespondM[P, Uo, Ui, Di, A])(f: A => RespondM[P, Uo, Ui, Di, B]): RespondM[P, Uo, Ui, Di, B] = fa.flatMap[Uo, Ui, B](f)(P)

  @inline override def point[A](a: => A): RespondM[P, Uo, Ui, Di, A] = RespondM.point[P, Di, A](a)(P)

  @inline override def join[A](ffa: RespondM[P, Uo, Ui, Di, RespondM[P, Uo, Ui, Di, A]]) = ffa.flatten[Ui, Uo, A](P, implicitly[RespondM[P, Uo, Ui, Di, A] <:< RespondM[P, Uo, Ui, Di, A]])

}

private[pipes] sealed trait RespondMPlus[P[+_, -_, -_, +_, +_], Uo, Ui, Di] extends Plus[({ type f[+dO] = RespondM[P, Uo, Ui, Di, dO] })#f] {
  implicit val P: Proxy[P]
  implicit val Di: Semigroup[Di]

  @inline override def plus[A](a: RespondM[P, Uo, Ui, Di, A], b: => RespondM[P, Uo, Ui, Di, A]): RespondM[P, Uo, Ui, Di, A] = a.plus[Uo, Ui, A](b)(P, Di)

}

private[pipes] sealed trait RespondMPlusEmpty[P[+_, -_, -_, +_, +_], Di, Ui, Uo] extends PlusEmpty[({ type f[+dO] = RespondM[P, Di, Ui, Uo, dO] })#f] with RespondMPlus[P, Uo, Ui, Di] {
  implicit override val Di: Monoid[Di]

  @inline override def empty[A]: RespondM[P, Uo, Ui, Di, A] = RespondM.empty[P, Di](P, Di)

}

private[pipes] sealed trait RespondMMonadPlus[P[+_, -_, -_, +_, +_], Di, Ui, Uo] extends MonadPlus[({ type f[+dO] = RespondM[P, Di, Ui, Uo, dO] })#f] with RespondMMonad[P, Uo, Ui, Di] with RespondMPlusEmpty[P, Uo, Ui, Di] {
  implicit override val P: Interact[P]

  @inline override def filter[A](fa: RespondM[P, Uo, Ui, Di, A])(f: A => Boolean): RespondM[P, Uo, Ui, Di, A] = fa.withFilter(f)(P, Di)

}
