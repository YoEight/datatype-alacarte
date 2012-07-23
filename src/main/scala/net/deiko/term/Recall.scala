package net.deiko.term

import net.deiko.control.Functor

case class Recall[A](f: Int => A)

object Recall {
  implicit val recallFunctor = new Functor[Recall]{
    def map[A, B](fa: Recall[A])(f: A => B) = fa match {
      case r@Recall(k) => r.copy(f compose k)
    }
  }
  
  implicit val recallRun = new Run[Recall]{
    def F = recallFunctor
    
    def runAlgebra[A](fa: Recall[(Mem => (A, Mem))], mem: Mem): (A, Mem) = (fa, mem) match {
      case (Recall(k), Mem(i)) => k(i)(mem)
    }
  }
}