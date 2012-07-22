package net.deiko.syntax

import net.deiko.control.Inject

trait InjectSyntax {
  type :<:[F[_], G[_]] = Inject[F, G]
}