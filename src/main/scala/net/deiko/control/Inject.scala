package net.deiko.control

trait :<:[F[_], G[_]] {
  def inj[A](sub: F[A]): G[A]
}
