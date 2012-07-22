package net.deiko.algebra

import net.deiko.control.Functor
import net.deiko.control.Inject
import net.deiko.syntax.inject._

case class Expr[F[_]](in: F[Expr[F]])

object Expr {
  def foldExpr[F[_], A](e: Expr[F],  f: F[A] => A)(implicit F: Functor[F]): A = 
    f(F.map(e.in)(expr => foldExpr(expr, f)))
    
  def eval[F[_]](e: Expr[F])(implicit E: Eval[F]): Int =
    foldExpr[F, Int](e, E.evalAlgebra)(E.F)
    
  def inject[F[_], G[_]](ge: F[Expr[G]])(implicit I: F :<: G): Expr[G] = Expr[G](I.inj(ge))
}