package ramjet

object ArrayBackend extends Backend[ArrayBackend.TImpl] {

  trait TLike[A]:
    def data: Array[A]
    def dims: Array[Int]
    inline def get(x: Int): A = data(x)
    inline def get(x: Int, y: Int): A = data(x * dims(0) + y)
    inline def set(x: Int, f: A): Unit = data(x) = f
    inline def set(x: Int, y: Int, f: A): Unit = data(x * dims(0) + y) = f

  enum TImpl:
    case F32(data: Array[Float], dims: Array[Int]) extends TImpl, TLike[Float]
    case F64(data: Array[Double], dims: Array[Int]) extends TImpl, TLike[Double]

  object TImpl:
    def f32(dims: Array[Int]): TImpl.F32 =
      TImpl.F32(new Array[Float](dims.product), dims)

    def f64(dims: Array[Int]): TImpl.F64 =
      TImpl.F64(new Array[Double](dims.product), dims)

  def unsafeF32(data: Array[Float], dims: Array[Int]): TImpl =
    val out = new Array[Float](data.length)
    System.arraycopy(data, 0, out, 0, data.length)
    TImpl.F32(out, dims)

  def unsafeF64(data: Array[Double], dims: Array[Int]): TImpl =
    val out = new Array[Double](data.length)
    System.arraycopy(data, 0, out, 0, data.length)
    TImpl.F64(out, dims)

  def vvf32(lhs: TImpl.F32, rhs: TImpl.F32): Float =
    var out = 0f
    (0 until lhs.dims(0)).foreach: i =>
      out += lhs.data(i) * rhs.data(i)
    out

  def vvf64(lhs: TImpl.F64, rhs: TImpl.F64): Double =
    var out = 0d
    (0 until lhs.dims(0)).foreach: i =>
      out += lhs.data(i) * rhs.data(i)
    out

  def scalarProduct[T <: Scalar](lhs: TImpl, rhs: TImpl): T =
    (lhs, rhs) match
      case (lhs: TImpl.F32, rhs: TImpl.F32) => vvf32(lhs, rhs).asInstanceOf[T]
      case (lhs: TImpl.F64, rhs: TImpl.F64) => vvf64(lhs, rhs).asInstanceOf[T]
      case _                                => throw IllegalArgumentException()

  def mmf32(lhs: TImpl.F32, rhs: TImpl.F32, out: TImpl.F32): Unit =
    (0 until lhs.dims(0)).foreach: i =>
      (0 until rhs.dims(1)).foreach: j =>
        var acc = 0f
        (0 until lhs.dims(1)).foreach: k =>
          acc += lhs.get(i, k) * rhs.get(k, j)
        out.set(i, j, acc)

  def mmf64(lhs: TImpl.F64, rhs: TImpl.F64, out: TImpl.F64): Unit =
    (0 until lhs.dims(0)).foreach: i =>
      (0 until rhs.dims(1)).foreach: j =>
        var acc = 0d
        (0 until lhs.dims(1)).foreach: k =>
          acc += lhs.get(i, k) * rhs.get(k, j)
        out.set(i, j, acc)

  def vmf32[A](lhs: TImpl.F32, rhs: TImpl.F32, out: TImpl.F32): Unit =
    (0 until rhs.dims(1)).foreach: j =>
      var acc = 0f
      (0 until rhs.dims(0)).foreach: i =>
        acc += lhs.get(i) * rhs.get(i, j)
      out.set(j, acc)

  def vmf64[A](lhs: TImpl.F64, rhs: TImpl.F64, out: TImpl.F64): Unit =
    (0 until rhs.dims(1)).foreach: j =>
      var acc = 0d
      (0 until rhs.dims(0)).foreach: i =>
        acc += lhs.get(i) * rhs.get(i, j)
      out.set(j, acc)

  def mvf32(lhs: TImpl.F32, rhs: TImpl.F32, out: TImpl.F32): Unit =
    (0 until lhs.dims(0)).foreach: i =>
      var acc = 0f
      (0 until lhs.dims(1)).foreach: j =>
        acc += lhs.get(i, j) * rhs.get(i)
      out.set(i, acc)

  def mvf64(lhs: TImpl.F64, rhs: TImpl.F64, out: TImpl.F64): Unit =
    (0 until lhs.dims(0)).foreach: i =>
      var acc = 0d
      (0 until lhs.dims(1)).foreach: j =>
        acc += lhs.get(i, j) * rhs.get(i)
      out.set(i, acc)

  def tensorProductF32(lhs: TImpl.F32, rhs: TImpl.F32): TImpl.F32 =
    (lhs.dims.length, rhs.dims.length) match
      case (2, 2) =>
        val out: TImpl.F32 = TImpl.f32(Array(lhs.dims(0), rhs.dims(1)))
        mmf32(lhs, rhs, out)
        out
      case (1, 2) =>
        val out = TImpl.f32(Array(rhs.dims(1)))
        vmf32(rhs, lhs, out)
        out

      case (2, 1) => {
        val out = TImpl.f32(Array(lhs.dims(0)))
        mvf32(rhs, lhs, out)
        out
      }
      case _ => throw IllegalArgumentException()

  def tensorProductF64(lhs: TImpl.F64, rhs: TImpl.F64): TImpl.F64 =
    (lhs.dims.length, rhs.dims.length) match
      case (2, 2) =>
        val out: TImpl.F64 = TImpl.f64(Array(lhs.dims(0), rhs.dims(1)))
        mmf64(lhs, rhs, out)
        out
      case (1, 2) =>
        val out = TImpl.f64(Array(rhs.dims(1)))
        vmf64(rhs, lhs, out)
        out

      case (2, 1) => {
        val out = TImpl.f64(Array(lhs.dims(0)))
        mvf64(rhs, lhs, out)
        out
      }
      case _ => throw IllegalArgumentException()

  def tensorProduct(lhs: TImpl, rhs: TImpl): TImpl =
    (lhs, rhs) match
      case (lhs: TImpl.F32, rhs: TImpl.F32) => tensorProductF32(lhs, rhs)
      case (lhs: TImpl.F64, rhs: TImpl.F64) => tensorProductF64(lhs, rhs)
      case _                                => throw IllegalArgumentException()

  def transposeF32(lhs: TImpl.F32): TImpl.F32 =
    // naive implementation that allocates
    val out = TImpl.f32(Array(lhs.dims(1), lhs.dims(0)))
    (0 until lhs.dims(0)).foreach: i =>
      (0 until lhs.dims(1)).foreach: j =>
        out.set(j, i, lhs.get(i, j))
    out

  def transposeF64(lhs: TImpl.F64): TImpl.F64 =
    // naive implementation that allocates
    val out = TImpl.f64(Array(lhs.dims(1), lhs.dims(0)))
    (0 until lhs.dims(0)).foreach: i =>
      (0 until lhs.dims(1)).foreach: j =>
        out.set(j, i, lhs.get(i, j))
    out

  def transpose(lhs: TImpl): TImpl =
    lhs match
      case lhs: TImpl.F32 => transposeF32(lhs)
      case lhs: TImpl.F64 => transposeF64(lhs)
      case _              => throw IllegalArgumentException()

  def invert(lhs: TImpl): TImpl = ???

  def append(lhs: TImpl, rhs: TImpl, axis: Int): TImpl = ???

  def reshape(t: TImpl, shape: Array[Int]): TImpl = t match
    case TImpl.F32(data, dims) => TImpl.F32(data, shape)
    case TImpl.F64(data, dims) => TImpl.F64(data, shape)

}
