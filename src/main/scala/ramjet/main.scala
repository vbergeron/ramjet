import ramjet.*

@main def run: Unit =

  val dsl = ArrayBackend.api
  import dsl.*

  val foo = unsafe(2, 2)(Array(0f, 1f, 1f, 0f))
  val bar = unsafe(2, 2)(Array(1f, 1f, 0f, 0f))
  val res = foo * bar * foo
  println(res.unwrap.data.mkString(", "))
