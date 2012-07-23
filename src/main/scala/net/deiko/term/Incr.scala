package net.deiko.term

import net.deiko.control.Functor

case class Incr[A](i: Int, v: A)

object Incr {
  implicit val incrFunctor = new Functor[Incr]{
    def map[A, B](fa: Incr[A])(f: A => B) = fa match {
      case Incr(i, a) => Incr(i, f(a))
    }
  }
  
  implicit val incrRun = new Run[Incr]{
    def F = incrFunctor
    
    def runAlgebra[A](fa: Incr[(Mem => (A, Mem))], mem: Mem): (A, Mem) = (fa, mem) match {
      case (Incr(k, f), Mem(i)) => f(Mem(i + k))
    }
  }
}