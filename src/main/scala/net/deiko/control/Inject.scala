package net.deiko.control

import net.deiko._

trait Inject[F[_], G[_]] {
  def inj[A](sub: F[A]): G[A]
}

object Inject extends InjectInstances {
  type :<:[F[_], G[_]] = Inject[F, G]
}

trait InjectInstances2 { 
  implicit def reflexiveInject[F[_]](implicit F: Functor[F]) = new Inject[F, F] { 
    def inj[A](sub: F[A]) = sub 
  }
}

trait InjectInstances1 extends InjectInstances2 { 
  implicit def leftKnownInject[F[_], G[_]](implicit F: Functor[F], G: Functor[G]) = new Inject[F, coproduct[F, G]#ap] {
    def inj[A](sub: F[A]): Either[F[A], G[A]] = Left(sub)
  }

  implicit def leftKnown3Inject[F[_], G[_], H[_]](implicit F: Functor[F], G: Functor[G], H: Functor[H]) = new Inject[F, coproduct[F, G]#or[H]#ap] { 
    def inj[A](sub: F[A]): Either[F[A], Either[G[A], H[A]]] = Left(sub)
  }
}

trait InjectInstances extends InjectInstances1 { 
  implicit def largerInject[F[_], G[_], H[_]](implicit F: Functor[F], G: Functor[G], H: Functor[H], I: Inject[F, G]) = new Inject[F, coproduct[H, G]#ap]{ 
    def inj[A](sub: F[A]): Either[H[A], G[A]] = Right(I.inj(sub))
  }
}
