package ramjet

import reflect.ClassTag
import compiletime.*
import compiletime.ops.int.*

final class TensorAPI[Scalar, Tensor](back: Backend[Scalar, Tensor]) {

  opaque type T0 = Scalar
  opaque type T1[N <: Int] = Tensor
  opaque type T2[N <: Int, M <: Int] = Tensor

  // Wraps a float array into a tensor
  inline def unsafe(n: Int)(data: Array[Scalar]): T1[n.type] =
    require(data.length == n, s"Provided array was not of size $n")
    back.unsafe(data, Array(n))

  // Wraps a float array into a tensor
  inline def unsafe(n: Int, m: Int)(data: Array[Scalar]): T2[n.type, m.type] =
    require(data.length == n * m, s"Provided array was not of size ${n * m}")
    back.unsafe(data, Array(n, m))

  // Wraps a float array into a tensor
  inline def fill(n: Int)(x: Scalar)(using ClassTag[Scalar]): T1[n.type] =
    unsafe(n)(Array.fill(n)(x))

  // Wraps a float array into a tensor
  inline def fill(n: Int, m: Int)(x: Scalar)(using ClassTag[Scalar]): T2[n.type, m.type] =
    unsafe(n, m)(Array.fill(n * m)(x))

  extension (x: T0) inline def unwrap: Scalar = x

  /* 1D Tensor API */
  extension [N <: Int](lhs: T1[N])

    inline def *[P <: Int, Q <: Int](rhs: T2[P, Q]): T1[P] =
      inline Dim.checkT1xT2[N, P, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def *[Q <: Int](rhs: T1[Q]): T0 =
      inline Dim.checkT1xT1[N, Q] match
        case true =>
          back.scalarProduct(lhs, rhs)

    inline def reshape(p: Int, q: Int): T2[p.type, q.type] =
      inline Dim.checkT1toT2[N, p.type, q.type] match
        case true => lhs

    inline def append0[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[P + 1, Q] =
      inline Dim.checkT1appendT2[N, P, Q](0) match
        case true => back.append(lhs, rhs, 0)

    inline def append1[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[P, Q + 1] =
      inline Dim.checkT1appendT2[N, P, Q](1) match
        case true => back.append(lhs, rhs, 1)

    inline def d0: N = constValue

    inline def unwrap: Tensor = lhs

  /* 2D Tensor API */
  extension [N <: Int, M <: Int](lhs: T2[N, M])

    inline def *[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[N, Q] =
      inline Dim.checkT2xT2[N, M, P, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def *[Q <: Int](rhs: T1[Q]): T1[N] =
      inline Dim.checkT2xT1[N, M, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def reshape(p: Int, q: Int): T2[p.type, q.type] =
      inline Dim.checkT2toT2[N, M, p.type, q.type] match
        case true => lhs

    inline def reshape(q: Int): T1[q.type] =
      inline Dim.checkT2toT1[N, M, q.type] match
        case true => lhs

    inline def append0[Q <: Int](rhs: T1[Q]): T2[N + 1, M] =
      inline Dim.checkT2appendT1[N, M, Q](0) match
        case true => back.append(lhs, rhs, 0)

    inline def append1[P <: Int](rhs: T1[P]): T2[N, M + 1] =
      inline Dim.checkT2appendT1[N, M, P](1) match
        case true => back.append(lhs, rhs, 1)

    inline def append0[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[N + P, M] =
      inline Dim.checkT2appendT2[N, M, P, Q](0) match
        case true => back.append(lhs, rhs, 0)

    inline def append1[P <: Int, Q <: Int](rhs: T2[P, Q]): T2[N, M + Q] =
      inline Dim.checkT2appendT2[N, M, P, Q](1) match
        case true => back.append(lhs, rhs, 1)

    inline def t: T2[M, N] =
      back.transpose(lhs)

    inline def inv: T2[N, M] =
      inline Dim.checkSquare[N, M] match
        case true => back.invert(lhs)

    inline def d0: N = constValue
    inline def d1: M = constValue

    inline def unwrap: Tensor = lhs
}
