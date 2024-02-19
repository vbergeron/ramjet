package ramjet.exemples

import ramjet.*
import compiletime.ops.int.*

@main def run: Unit =

  import ArrayBackend.api.*

  val foo = unsafe(2, 2)(Array(0f, 1f, 1f, 0f))
  val bar = unsafe(2, 2)(Array(1f, 1f, 0f, 0f))
  val res = foo * bar * foo

  println(res.unwrap.data.mkString(", "))
