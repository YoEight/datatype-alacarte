package net.deiko.term

import net.deiko.coproduct
import net.deiko.control.Functor

trait Run[F[_]] {
  def F: Functor[F]

  def runAlgebra[A](fa: F[(Mem => (A, Mem))], mem: Mem): (A, Mem)
}

object Run {
  private implicit def evalFunctor[F[_]](implicit F: Run[F]) = F.F
  
  implicit def coprodRun[F[_], G[_]](implicit F0: Run[F], G0: Run[G]) = new Run[coproduct[F, G]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#ap]]

    def runAlgebra[A](ea: Either[F[Mem => (A, Mem)], G[Mem => (A, Mem)]], mem: Mem): (A, Mem) = ea match {
      case Left(fa)  => F0.runAlgebra(fa, mem)
      case Right(ga) => G0.runAlgebra(ga, mem)
    }
  }
  
  implicit def coprod3Run[F[_], G[_], H[_]](implicit F0: Run[F], G0: Run[G], H0: Run[H]) = new Run[coproduct[F, G]#or[H]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#or[H]#ap]]

    def runAlgebra[A](ea: Either[F[Mem => (A, Mem)], Either[G[Mem => (A, Mem)], H[Mem => (A, Mem)]]], mem: Mem): (A, Mem) = ea match {
      case Left(fa)   => F0.runAlgebra(fa, mem)
      case Right(gha) => coprodRun[G, H](G0, H0).runAlgebra(gha, mem)
    }
  }
}