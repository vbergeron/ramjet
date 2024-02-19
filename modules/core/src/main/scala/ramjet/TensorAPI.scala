package ramjet

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

  extension (x: T0) inline def unwrap: Scalar = x

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

    inline def unwrap: Tensor = lhs

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

    inline def t: T2[M, N] =
      back.transpose(lhs)

    inline def unwrap: Tensor = lhs
}
