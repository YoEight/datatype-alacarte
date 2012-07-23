package net.deiko.app

import net.deiko.coproduct
import net.deiko.syntax.inject._
import net.deiko.term._
import Term._

object Free extends App {
  
  def run[F[_], A](t: Term[F, A], mem: Mem)(implicit F: Run[F]): (A, Mem) = 
    foldTerm[F, A, Mem => (A, Mem)](t, (a: A) => (b: Mem) => (a, b), (fb: F[Mem => (A, Mem)]) => (i: Mem) => F.runAlgebra[A](fb, i))(F.F).apply(mem)
    
  def incr[F[_]](i: Int)(implicit I: Incr :<: F): Term[F, Unit] =
    inject[Incr, F, Unit](Incr(i, Pure[F, Unit]()))
    
  def recall[F[_]](implicit I: Recall :<: F): Term[F, Int] =
    inject[Recall, F, Int](Recall(i => Pure[F, Int](i)))
  
  override def main(args: Array[String]) {
    val tick: Term[coproduct[Recall, Incr]#ap, Int] = for { 
      y <- recall[coproduct[Recall, Incr]#ap]
      _ <- incr[coproduct[Recall, Incr]#ap](1)
    } yield y

    println(run[coproduct[Recall, Incr]#ap, Int](tick, Mem(4)))
  }
}