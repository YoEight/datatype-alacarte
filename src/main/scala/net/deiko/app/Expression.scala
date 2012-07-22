package net.deiko.app

import net.deiko.algebra._
import net.deiko.syntax.inject._
import net.deiko.coproduct
import Expr._

object Expression extends App {
  
  def constant[F[_]](n: Int)(implicit I: Val :<: F): Expr[F] = inject[Val, F](Val(n))
  def plus[F[_]](l: Expr[F], r: Expr[F])(implicit I: Add :<: F) = inject[Add, F](Add(l, r))
  def mul[F[_]](l: Expr[F], r: Expr[F])(implicit I: Mul :<: F) = inject[Mul, F](Mul(l, r))

  override def main(args: Array[String]) {
    val addExample: Expr[coproduct[Val, Add]#ap] = 
      Expr[coproduct[Val, Add]#ap](Right(Add(Expr[coproduct[Val, Add]#ap](Left(Val(118))), Expr[coproduct[Val, Add]#ap](Left(Val(1219))))))
      
    println(eval[coproduct[Val, Add]#ap](addExample))
    
    // 30 000 + 1 330 + 7
    val magic: Expr[coproduct[Add, Val]#ap] = 
      plus[coproduct[Add, Val]#ap](constant[coproduct[Add, Val]#ap](30000), plus[coproduct[Add, Val]#ap](constant[coproduct[Add, Val]#ap](1330), constant[coproduct[Add, Val]#ap](7)))
      
    println(eval[coproduct[Add, Val]#ap](magic))
    
    // (80 * 5) + 4
    val anotherMagic: Expr[coproduct[Val, Add]#or[Mul]#ap] = 
      plus[coproduct[Val, Add]#or[Mul]#ap](mul[coproduct[Val, Add]#or[Mul]#ap](constant[coproduct[Val, Add]#or[Mul]#ap](80), constant[coproduct[Val, Add]#or[Mul]#ap](5)), constant[coproduct[Val, Add]#or[Mul]#ap](4))
      
    println(eval[coproduct[Val, Add]#or[Mul]#ap](anotherMagic))
    
    // 6 * 7
    val oneMore: Expr[coproduct[Val, Mul]#ap] =
      mul[coproduct[Val, Mul]#ap](constant[coproduct[Val, Mul]#ap](6), constant[coproduct[Val, Mul]#ap](7))
      
    println(eval[coproduct[Val, Mul]#ap](oneMore))
  }
}