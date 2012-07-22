package net.deiko.control

import net.deiko._

trait Functor[F[_]]{
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor { 
  implicit def coproductFunctor[F[_], G[_]](implicit F: Functor[F], G: Functor[G]) = new Functor[coproduct[F, G]#ap] { 
    def map[A, B](ea: Either[F[A], G[A]])(f: A => B) = ea match { 
      case Left(fa)  => Left(F.map(fa)(f))
      case Right(ga) => Right(G.map(ga)(f))
    }
  }

  implicit def coproduct3Functor[F[_], G[_], H[_]](implicit F: Functor[F], G: Functor[G], H: Functor[H]) = new Functor[coproduct[F, G]#or[H]#ap] {
    def map[A, B](ea: Either[F[A], Either[G[A], H[A]]])(f: A => B) = ea match { 
      case Left(fa) => Left(F.map(fa)(f))
      case Right(o) => Right(coproductFunctor[G,H].map(o)(f))
    } 
  } 
}
