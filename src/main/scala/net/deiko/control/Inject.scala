package net.deiko.control

trait Inject[F[_], G[_]] {
  def inj[A](sub: F[A]): G[A]
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
