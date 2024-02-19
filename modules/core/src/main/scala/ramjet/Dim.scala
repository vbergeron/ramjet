package ramjet

import compiletime.*
import compiletime.ops.any.*
import compiletime.ops.string.*
import compiletime.ops.int.`*`
import compiletime.ops.any.ToString as Str

object Dim {

  // helper to format dimentions to a readable string
  inline def fmt[I <: Int]: "[" + Str[I] + "]" = constValue
  inline def fmt[I <: Int, J <: Int]: "[" + Str[I] + ", " + Str[J] + "]" = constValue

  // Checks if we can reshape an 2d tensor into another one
  inline def checkT2toT2[N <: Int, M <: Int, P <: Int, Q <: Int]: Boolean =
    inline if constValue[N * M == P * Q] then true
    else error("Error reshaping " + fmt[N, M] + " to " + fmt[P, Q])

  // Checks if we can reshape an 1d tensor into a 2d one
  inline def checkT1toT2[N <: Int, P <: Int, Q <: Int]: Boolean =
    inline if constValue[N == P * Q] then true
    else error("Error reshaping " + fmt[N] + " to " + fmt[P, Q])

  // Checks if we can reshape an 1d tensor into a 2d one
  inline def checkT2toT1[N <: Int, M <: Int, Q <: Int]: Boolean =
    inline if constValue[N * M == Q] then true
    else error("Error reshaping " + fmt[N, M] + " to " + fmt[Q])

  // Checks if 2 2d tensors can be multiplied together
  inline def checkT2xT2[N <: Int, M <: Int, P <: Int, Q <: Int]: Boolean =
    inline if constValue[M == P] then true
    else error("Wrong tensor format: " + fmt[N, M] + " x " + fmt[P, Q])

  inline def checkT1xT1[N <: Int, Q <: Int]: Boolean =
    inline if constValue[N == Q] then true
    else error("Wrong tensor format: " + fmt[N] + " x " + fmt[Q])

  inline def checkT1xT2[N <: Int, P <: Int, Q <: Int]: Boolean =
    inline if constValue[N == P] then true
    else error("Wrong tensor format: " + fmt[N] + " x " + fmt[P, Q])

  // Checks if 2 2d tensors can be multiplied together
  inline def checkT2xT1[N <: Int, M <: Int, Q <: Int]: Boolean =
    inline if constValue[M == Q] then true
    else error("Wrong tensor format: " + fmt[N, M] + " x " + fmt[Q])

  inline def checkSquare[N <: Int, M <: Int]: Boolean =
    inline if constValue[N == M] then true
    else error("Not a square 2D tensor : " + fmt[N, M])

  inline def checkT2appendT1[N <: Int, M <: Int, Q <: Int](axis: 0 | 1): Boolean =
    inline axis match
      case 0 =>
        inline if constValue[M == Q] then true
        else error(fmt[N, M] + " does not have " + fmt[Q] + " columns")
      case 1 =>
        inline if constValue[N == Q] then true
        else error(fmt[N, M] + " does not have " + fmt[Q] + " lines")

  inline def checkT2appendT2[N <: Int, M <: Int, P <: Int, Q <: Int](axis: 0 | 1): Boolean =
    inline axis match
      case 0 =>
        inline if constValue[M == Q] then true
        else error(fmt[N, M] + " and " + fmt[P, Q] + " have not the same number of columns")
      case 1 =>
        inline if constValue[N == P] then true
        else error(fmt[N, M] + " and " + fmt[P, Q] + " have not the same number of lines")

  inline def checkT1appendT2[N <: Int, P <: Int, Q <: Int](axis: 0 | 1): Boolean =
    inline axis match
      case 0 =>
        inline if constValue[N == Q] then true
        else error(fmt[P, Q] + " does not have " + fmt[N] + " columns")
      case 1 =>
        inline if constValue[N == P] then true
        else error(fmt[P, Q] + " does not have " + fmt[N] + " lines")
}
