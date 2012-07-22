package net.deiko.algebra

import net.deiko.control.Functor

case class Mul[A](left: A, right: A)

object Mul {
  implicit val mulFunctor = new Functor[Mul]{
    def map[A, B](fa: Mul[A])(f: A => B) = fa match {
      case Mul(l, r) => Mul(f(l), f(r))
    }
  }
  
  implicit val mulEval = new Eval[Mul]{
    def F = mulFunctor
    
    def evalAlgebra(fa: Mul[Int]) = fa match {
      case Mul(l, r) => l * r
    }
  }
}