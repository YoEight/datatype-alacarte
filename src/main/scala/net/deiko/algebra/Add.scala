package net.deiko.algebra

import net.deiko.control.Functor

case class Add[A](left: A, right: A)

object Add {
  implicit val addFunctor = new Functor[Add]{
    def map[A, B](fa: Add[A])(f: A => B) = fa match {
      case Add(l, r) => Add(f(l), f(r))
    }
  }
  
  implicit val addEval = new Eval[Add]{
    def F = addFunctor
    
    def evalAlgebra(fa: Add[Int]) = fa match {
      case Add(l, r) => l + r
    }
  }
}