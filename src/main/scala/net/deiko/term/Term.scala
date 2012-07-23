package net.deiko.term

import net.deiko.control.Functor

// Free Monad
trait Term[F[_], A] {
  def fold[Z](pure: A => Z, impure: F[Term[F, A]] => Z): Z
  
  def map[B](f: A => B)(implicit F: Functor[F]): Term[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Term[F, B])(implicit F: Functor[F]): Term[F, B] = this match {
    case Pure(a) => f(a)
    case Impure(fa) => Impure[F, B](F.map(fa)(_ flatMap f))
  }
}

object Term {
  implicit def termFunctor[F[_]](implicit F: Functor[F]) = new Functor[({type f[x] = Term[F, x]})#f]{
    def map[A, B](fa: Term[F, A])(f: A => B) = fa map f
  }
}

object Pure {
  def unapply[F[_], A](t: Term[F, A]): Option[A] =
    t.fold(Some(_), _ => None)

  def apply[F[_], A](v: A): Term[F, A] = new Term[F, A] {
    def fold[Z](pure: A => Z, impure: F[Term[F, A]] => Z): Z = pure(v)
  }
}

object Impure {
  def unapply[F[_], A](t: Term[F, A]): Option[F[Term[F, A]]] =
    t.fold(_ => None, Some(_))

  def apply[F[_], A](fa: F[Term[F, A]]): Term[F, A] = new Term[F, A] {
    def fold[Z](pure: A => Z, impure: F[Term[F, A]] => Z): Z = impure(fa)
  }
}