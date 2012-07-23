package net.deiko.term

import net.deiko.control.Functor
import net.deiko.syntax.inject._

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
  def inject[F[_], G[_], A](fe: F[Term[G, A]])(implicit I: F :<: G): Term[G, A] =
    Impure[G, A](I.inj(fe))
    
  def foldTerm[F[_], A, B](t: Term[F, A], pure: A => B, impure: F[B] => B)(implicit F: Functor[F]): B = t match {
    case Pure(a)    => pure(a)
    case Impure(fb) => impure(F.map(fb)(term => foldTerm(term, pure, impure)))
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