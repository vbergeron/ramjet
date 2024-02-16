package ramjet

import compiletime.*
import compiletime.ops.int.*

object Tensor {

  // TODO adapt for different backends
  opaque type T2[N <: Int, M <: Int] = Array[Float]

  // Wraps a float array into a tensor
  inline def unsafe(n: Int, m: Int)(data: Array[Float]): T2[n.type, m.type] =
    require(data.length == n * m, s"Provided array was not of size ${n * m}")
    val out = new Array[Float](data.length)
    println(out.length)
    System.arraycopy(data, 0, out, 0, data.length)
    out

  extension [N <: Int, M <: Int](lhs: T2[N, M])

    inline def *[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[N, Q] =
      inline Dim.checkProduct[N, M, P, Q] match
        case true =>
          val out = new Array[Float](Dim.elems[N, Q])
          (0 until constValue[N]).foreach: i =>
            (0 until constValue[Q]).foreach: j =>
              var acc = 0f
              (0 until constValue[M]).foreach: k =>
                acc += lhs(Dim.offset[N](i, k)) * rhs(Dim.offset[P](k, j))
              out(Dim.offset[N](i, j)) = acc
          out

    inline def reshape(p: Int, q: Int): T2[p.type, q.type] =
      inline Dim.checkReshape[N, M, p.type, q.type] match
        case true => lhs

    inline def unwrap: Array[Float] = lhs
}
