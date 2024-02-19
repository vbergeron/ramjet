package ramjet

object ArrayBackend extends Backend[Float, ArrayBackend.T] {

  final case class T(data: Array[Float], dims: Array[Int]):
    inline def get(x: Int): Float = data(x)
    inline def get(x: Int, y: Int): Float = data(x * dims(0) + y)
    inline def set(x: Int, f: Float): Unit = data(x) = f
    inline def set(x: Int, y: Int, f: Float): Unit = data(x * dims(0) + y) = f

  type Scalar = Float
  type Tensor = T

  def unsafe(data: Array[Scalar], dims: Array[Int]): Tensor =
    val out = new Array[Float](data.length)
    System.arraycopy(data, 0, out, 0, data.length)
    T(out, dims)

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
      case (1, 2) =>
        val out = T(new Array[Float](rhs.dims(1)), Array(rhs.dims(1)))
        (0 until rhs.dims(1)).foreach: j =>
          var acc = 0f
          (0 until rhs.dims(0)).foreach: i =>
            acc += lhs.get(i) * rhs.get(i, j)
          out.set(j, acc)
        out
      case (2, 1) => {
        val out = T(new Array[Float](lhs.dims(0)), Array(lhs.dims(0)))
        (0 until lhs.dims(0)).foreach: i =>
          var acc = 0f
          (0 until lhs.dims(1)).foreach: j =>
            acc += lhs.get(i, j) * rhs.get(i)
          out.set(i, acc)
        out
      }
      case _ => throw IllegalArgumentException()
}
