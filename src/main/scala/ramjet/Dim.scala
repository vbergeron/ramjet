package ramjet

import compiletime.*
import compiletime.ops.any.*
import compiletime.ops.string.*
import compiletime.ops.int.`*`
import compiletime.ops.int.ToString as Str

object Dim {
  inline def apply[I <: Int, J <: Int]: (I, J) =
    inline val I = constValue[I]
    inline val J = constValue[J]
    (I, J)

  // helper to format dimentions to a readable string
  inline def fmt[I <: Int, J <: Int]: "[" + Str[I] + ", " + Str[J] + "]" = constValue

  // Get the maximum number of elements of a 2 dim tensor
  inline def elems[I <: Int, J <: Int]: I * J = constValue

  // Compute the offset using the 2 indices
  inline def offset[I <: Int](i: Int, j: Int): Int =
    inline val I = constValue[I]
    i * I + j

  // Checks if we can reshape an 2d tensor into another one
  inline def checkReshape[N <: Int, M <: Int, P <: Int, Q <: Int]: Boolean =
    inline if elems[N, M] == elems[P, Q] then true
    else
      inline val lhs = fmt[N, M]
      inline val rhs = fmt[P, Q]
      error("Error reshaping " + lhs + " to " + rhs)

  // Checks if 2 2d tensors can be multiplied together
  inline def checkProduct[N <: Int, M <: Int, P <: Int, Q <: Int]: Boolean =
    inline val N: Int = constValue[N]
    inline val M: Int = constValue[M]
    inline val P: Int = constValue[P]
    inline val Q: Int = constValue[Q]

    inline if M == P then true
    else
      inline val lhs = fmt[N, M]
      inline val rhs = fmt[P, Q]
      error("Wrong tensor format: " + lhs + " x " + rhs)

}
