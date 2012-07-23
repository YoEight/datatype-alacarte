package net.deiko.term

import net.deiko.coproduct
import net.deiko.control.Functor

trait Run[F[_]] {
  def F: Functor[F]

  def runAlgebra[A](fa: F[(Int => (A, Int))], mem: Int): (A, Int)
}

object Run {
  private implicit def evalFunctor[F[_]](implicit F: Run[F]) = F.F
  
  implicit def coprodRun[F[_], G[_]](implicit F0: Run[F], G0: Run[G]) = new Run[coproduct[F, G]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#ap]]

    def runAlgebra[A](ea: Either[F[Int => (A, Int)], G[Int => (A, Int)]], mem: Int): (A, Int) = ea match {
      case Left(fa)  => F0.runAlgebra(fa, mem)
      case Right(ga) => G0.runAlgebra(ga, mem)
    }
  }
  
  implicit def coprod3Run[F[_], G[_], H[_]](implicit F0: Run[F], G0: Run[G], H0: Run[H]) = new Run[coproduct[F, G]#or[H]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#or[H]#ap]]

    def runAlgebra[A](ea: Either[F[Int => (A, Int)], Either[G[Int => (A, Int)], H[Int => (A, Int)]]], mem: Int): (A, Int) = ea match {
      case Left(fa)   => F0.runAlgebra(fa, mem)
      case Right(gha) => coprodRun[G, H](G0, H0).runAlgebra(gha, mem)
    }
  }
}