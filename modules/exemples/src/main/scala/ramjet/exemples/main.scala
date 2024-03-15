package ramjet.exemples

import ramjet.*
import compiletime.ops.int.*
import ramjet.backend.storch.StorchBackend

@main def run: Unit =

  val backend = StorchBackend
  import backend.api.*

  val foo = unsafe(2, 2)(Array(0f, 1f, 1f, 0f))
  // val bar = unsafe(2, 2)(Array(1f, 1f, 0f, 0f))
  val bar = unsafe(2)(Array(1f, 1f))

  val res = foo.append0(bar)

  println(backend.floatArray(res.unwrap).toList)
