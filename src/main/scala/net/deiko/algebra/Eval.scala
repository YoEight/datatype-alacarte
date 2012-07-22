package net.deiko.algebra

import net.deiko._
import net.deiko.control.Functor

trait Eval[F[_]] {
  def F: Functor[F] 
  
  def evalAlgebra(fa: F[Int]): Int
}

object Eval extends EvalInstances

trait EvalInstances {
  private implicit def evalFunctor[F[_]](implicit F: Eval[F]) = F.F

  implicit def coprodEval[F[_], G[_]](implicit F0: Eval[F], G0: Eval[G]) = new Eval[coproduct[F, G]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#ap]]
    
    def evalAlgebra(ea: Either[F[Int], G[Int]]) = ea match {
      case Left(fa)  => F0.evalAlgebra(fa)
      case Right(ga) => G0.evalAlgebra(ga)
    }
  }
  
  implicit def coprod3Eval[F[_], G[_], H[_]](implicit F0: Eval[F], G0: Eval[G], H0: Eval[H]) = new Eval[coproduct[F, G]#or[H]#ap]{
    def F = implicitly[Functor[coproduct[F, G]#or[H]#ap]]

    def evalAlgebra(ea: Either[F[Int], Either[G[Int], H[Int]]]) = ea match { 
      case Left(fa)   => F0.evalAlgebra(fa)
      case Right(gha) => coprodEval[G, H](G0, H0).evalAlgebra(gha)
    }
  }
}
