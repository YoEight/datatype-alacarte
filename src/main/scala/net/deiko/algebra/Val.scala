package net.deiko.algebra

import net.deiko.control.Functor

case class Val[A](v: Int)

object Val {
  implicit val valFunctor = new Functor[Val]{
    def map[A, B](fa: Val[A])(f: A => B) = Val(fa.v)
  }
  
  implicit val valEval = new Eval[Val]{
    def F = valFunctor
    
    def evalAlgebra(fa: Val[Int]) = fa.v
  }
}