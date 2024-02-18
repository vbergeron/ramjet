package ramjet

object ArrayBackend extends Backend {

  final case class T(data: Array[Float], dims: Array[Int]):
    inline def get(x: Int, y: Int): Float = data(x * dims(0) + y)
    inline def set(x: Int, y: Int, f: Float): Unit = data(x * dims(0) + y) = f

  type Scalar = Float
  type Tensor = T

  def unsafe(data: Array[Scalar]): Tensor =
    val out = new Array[Float](data.length)
    System.arraycopy(data, 0, out, 0, data.length)
    T(out, Array(out.length))

  def scalarProduct(lhs: Tensor, rhs: Tensor): Scalar =
    var out = 0f
    (0 until lhs.dims(0)).foreach: i =>
      out += lhs.data(i) * rhs.data(i)
    out

  def tensorProduct(lhs: T, rhs: T): Tensor =
    (lhs.dims.length, rhs.dims.length) match
      case (2, 2) =>
        val out = T(new Array[Float](lhs.dims(0) * rhs.dims(1)), Array(lhs.dims(0), rhs.dims(1)))
        (0 until lhs.dims(0)).foreach: i =>
          (0 until rhs.dims(1)).foreach: j =>
            var acc = 0f
            (0 until lhs.dims(1)).foreach: k =>
              acc += lhs.get(i, k) * rhs.get(k, j)
            out.set(i, j, acc)
        out
      case _ => throw IllegalArgumentException()
}
