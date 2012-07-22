package net.deiko.term

import net.deiko.control.Functor

// Free Monad
sealed trait Term[F[_], A] {
  import Term._
  
  def map[B](f: A => B)(implicit F: Functor[F]): Term[F, B] = flatMap(a => Pure(f(a)))
  
  def flatMap[B](f: A => Term[F, B])(implicit F: Functor[F]): Term[F, B] = this match {
    case Pure(v)    => Pure(f(v))
    case Impure(fa) => Impure(F.map(fa)(f))
  }
}

object Term {
  case class Pure[F[_], A](v: A) extends Term[F, A]
  case class Impure[F[_], A](v: F[Term[F, A]]) extends Term[F, A]
}