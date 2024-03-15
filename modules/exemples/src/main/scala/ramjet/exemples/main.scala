package ramjet.exemples

import ramjet.*
import compiletime.ops.int.*
import ramjet.backend.storch.StorchBackend

@main def run: Unit =

  val backend = StorchBackend
  import backend.api.*

  val foo = unsafe(3, 2)(Array(1f, 1f, 1f, 1f, 1f, 1f))

  val res = foo.append(foo, 1)

  println(backend.floatArray(res.unwrap).toList)
