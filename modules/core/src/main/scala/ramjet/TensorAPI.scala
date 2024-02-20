package ramjet

import reflect.ClassTag
import compiletime.*
import compiletime.ops.int.*

final class TensorAPI[Tensor](back: Backend[Tensor]) {

  opaque type T1[T <: Scalar, N <: Int] = Tensor
  opaque type T2[T <: Scalar, N <: Int, M <: Int] = Tensor

  // Wraps a float array into a tensor
  def unsafe[T <: Scalar](n: Int)(data: Array[T]): T1[T, n.type] =
    require(data.length == n, s"Provided array was not of size $n")
    data match
      case data: Array[Float]  => back.unsafeF32(data, Array(n))
      case data: Array[Double] => back.unsafeF64(data, Array(n))

  // Wraps a float array into a tensor
  def unsafe[T <: Scalar](n: Int, m: Int)(data: Array[T]): T2[T, n.type, m.type] =
    require(data.length == n * m, s"Provided array was not of size ${n * m}")
    data match
      case data: Array[Float]  => back.unsafeF32(data, Array(n, m))
      case data: Array[Double] => back.unsafeF64(data, Array(n, m))

  // Wraps a float array into a tensor
  inline def fill[T <: Scalar](n: Int)(x: T)(using ClassTag[T]): T1[T, n.type] =
    unsafe(n)(Array.fill(n)(x))

  // Wraps a float array into a tensor
  inline def fill[T <: Scalar](n: Int, m: Int)(x: T)(using ClassTag[T]): T2[T, n.type, m.type] =
    unsafe(n, m)(Array.fill(n * m)(x))

  type Select1[I <: 0 | 1, Case0, Case1] = I match
    case 0 => Case0
    case 1 => Case1

  /* 1D Tensor API */
  extension [T <: Scalar, N <: Int](lhs: T1[T, N])

    inline def *[P <: Int, Q <: Int](rhs: T2[T, P, Q]): T1[T, P] =
      inline Dim.checkT1xT2[N, P, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def *[Q <: Int](rhs: T1[T, Q]): T =
      inline Dim.checkT1xT1[N, Q] match
        case true =>
          back.scalarProduct(lhs, rhs)

    inline def reshape(p: Int, q: Int): T2[T, p.type, q.type] =
      inline Dim.checkT1toT2[N, p.type, q.type] match
        case true => back.reshape(lhs, Array(p, q))

    inline def append[P <: Int, Q <: Int](
        rhs: T2[T, P, Q],
        axis: 0 | 1
    ): Select1[axis.type, T2[T, P + 1, Q], T2[T, P, Q + 1]] =
      inline axis match
        case 0 =>
          inline Dim.checkT1appendT2[N, P, Q](0) match
            case true => back.append(lhs, rhs, 0).asInstanceOf
        case 1 =>
          inline Dim.checkT1appendT2[N, P, Q](1) match
            case true => back.append(lhs, rhs, 1).asInstanceOf

    inline def d0: N = constValue

    inline def unwrap: Tensor = lhs

  /* 2D Tensor API */
  extension [T <: Scalar, N <: Int, M <: Int](lhs: T2[T, N, M])

    inline def *[P <: Int, Q <: Int](rhs: T2[T, P, Q]): T2[T, N, Q] =
      inline Dim.checkT2xT2[N, M, P, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def *[Q <: Int](rhs: T1[T, Q]): T1[T, N] =
      inline Dim.checkT2xT1[N, M, Q] match
        case true =>
          back.tensorProduct(lhs, rhs)

    inline def reshape(p: Int, q: Int): T2[T, p.type, q.type] =
      inline Dim.checkT2toT2[N, M, p.type, q.type] match
        case true => back.reshape(lhs, Array(p, q))

    inline def reshape(q: Int): T1[T, q.type] =
      inline Dim.checkT2toT1[N, M, q.type] match
        case true => back.reshape(lhs, Array(q))

    inline def append[Q <: Int](
        rhs: T1[T, Q],
        axis: 0 | 1
    ): Select1[axis.type, T2[T, N + 1, M], T2[T, N, M + 1]] =
      inline axis match
        case 0 =>
          inline Dim.checkT2appendT1[N, M, Q](0) match
            case true => back.append(lhs, rhs, 0).asInstanceOf // safe since we checked
        case 1 =>
          inline Dim.checkT2appendT1[N, M, Q](1) match
            case true => back.append(lhs, rhs, 1).asInstanceOf // safe since we checked

    inline def append[P <: Int, Q <: Int](
        rhs: T2[T, P, Q],
        axis: 0 | 1
    ): Select1[axis.type, T2[T, N + P, M], T2[T, N, M + Q]] =
      inline axis match
        case 0 =>
          inline Dim.checkT2appendT2[N, M, P, Q](0) match
            case true => back.append(lhs, rhs, 0).asInstanceOf // safe since we checked
        case 1 =>
          inline Dim.checkT2appendT2[N, M, P, Q](1) match
            case true => back.append(lhs, rhs, 1).asInstanceOf // safe since we checked

    inline def t: T2[T, M, N] =
      back.transpose(lhs)

    inline def inv: T2[T, N, M] =
      inline Dim.checkSquare[N, M] match
        case true => back.invert(lhs)

    inline def d0: N = constValue
    inline def d1: M = constValue

    inline def unwrap: Tensor = lhs
}
