package ramjet.exemples

import ramjet.*

@main def run: Unit =

  import ArrayBackend.api.*

  val foo = unsafe(2, 2)(Array(0f, 1f, 1f, 0f))
  val bar = unsafe(2, 2)(Array(1f, 1f, 0f, 0f))
  val res = foo * bar * foo

  val baz = fill(2, 10)(1f)
  baz.reshape(5, 5)

  println(res.unwrap.data.mkString(", "))
