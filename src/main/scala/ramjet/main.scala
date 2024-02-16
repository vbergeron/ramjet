import ramjet.*

@main def run =
  def foo = Tensor.unsafe(2, 2)(Array(3, 4, 2, 1))
  def bar = Tensor.unsafe(2, 2)(Array(1, 2, 3, 4))
  def res = foo * bar

  println(res.unwrap.mkString(", "))

  // println(res.dim.fmt)
