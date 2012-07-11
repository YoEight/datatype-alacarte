package net.deiko

trait coproduct[F[_], G[_]] { 
  type or[H[_]] = coproduct[F[_], ({type f[x] = Either[G[x], H[x]]})#f]
  type ap[A] = Either[F[A], G[A]]
}
