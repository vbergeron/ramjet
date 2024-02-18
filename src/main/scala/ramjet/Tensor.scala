package ramjet

import compiletime.*
import compiletime.ops.int.*

object Tensor {

  // TODO adapt for different backends
  opaque type T0 = Float
  opaque type T1[N <: Int] = Array[Float]
  opaque type T2[N <: Int, M <: Int] = Array[Float]

  // Wraps a float array into a tensor
  inline def unsafe(n: Int)(data: Array[Float]): T1[n.type] =
    require(data.length == n, s"Provided array was not of size $n")
    val out = new Array[Float](data.length)
    println(out.length)
    System.arraycopy(data, 0, out, 0, data.length)
    out

  // Wraps a float array into a tensor
  inline def unsafe(n: Int, m: Int)(data: Array[Float]): T2[n.type, m.type] =
    require(data.length == n * m, s"Provided array was not of size ${n * m}")
    val out = new Array[Float](data.length)
    println(out.length)
    System.arraycopy(data, 0, out, 0, data.length)
    out

  extension (x: T0) inline def unwrap: Float = x

  extension [N <: Int](lhs: T1[N])

    inline def *[P <: Int, Q <: Int](rhs: T2[P, Q]): T1[P] =
      inline Dim.checkT1xT2[N, P, Q] match
        case true =>
          val out = new Array[Float](constValue[P])
          (0 until constValue[P]).foreach: k =>
            var acc = 0f
            (0 until constValue[N]).foreach: i =>
              acc += lhs(i) * rhs(Dim.offset[P](i, k))
            out(k) = acc
          out

    inline def *[Q <: Int](rhs: T1[Q]): T0 =
      inline Dim.checkT1xT1[N, Q] match
        case true =>
          var out = 0f
          (0 until constValue[N]).foreach: i =>
            out += lhs(i) * rhs(i)
          out

    inline def reshape(p: Int, q: Int): T2[p.type, q.type] =
      inline Dim.checkT1toT2[N, p.type, q.type] match
        case true => lhs

    inline def unwrap: Array[Float] = lhs

  extension [N <: Int, M <: Int](lhs: T2[N, M])

    inline def *[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[N, Q] =
      inline Dim.checkT2xT2[N, M, P, Q] match
        case true =>
          val out = new Array[Float](Dim.elems[N, Q])
          (0 until constValue[N]).foreach: i =>
            (0 until constValue[Q]).foreach: j =>
              var acc = 0f
              (0 until constValue[M]).foreach: k =>
                acc += lhs(Dim.offset[N](i, k)) * rhs(Dim.offset[P](k, j))
              out(Dim.offset[N](i, j)) = acc
          out

    inline def *[Q <: Int](rhs: T1[Q]): T1[N] =
      inline Dim.checkT2xT1[N, M, Q] match
        case true =>
          val out = new Array[Float](constValue[N])
          (0 until constValue[N]).foreach: i =>
            var acc = 0f
            (0 until constValue[M]).foreach: k =>
              acc += lhs(Dim.offset[N](i, k)) * rhs(k)
            out(i) = acc
          out

    inline def reshape(p: Int, q: Int): T2[p.type, q.type] =
      inline Dim.checkT2toT2[N, M, p.type, q.type] match
        case true => lhs

    inline def reshape(q: Int): T1[q.type] =
      inline Dim.checkT2toT1[N, M, q.type] match
        case true => lhs

    inline def unwrap: Array[Float] = lhs
}
