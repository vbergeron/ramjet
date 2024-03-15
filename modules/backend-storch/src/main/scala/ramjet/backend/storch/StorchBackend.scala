package ramjet.backend.storch

import ramjet.Backend
import org.bytedeco.openblas.global.openblas
import torch.Float32
import org.bytedeco.pytorch.Tensor
import org.bytedeco.pytorch.global.torch.ScalarType
import org.bytedeco.pytorch.global
import org.bytedeco.pytorch.TensorArrayRef
import org.bytedeco.pytorch.TensorVector
import java.util.Arrays
import scala.annotation.switch

object StorchBackend extends Backend[Tensor] {
  def unsafeF32(data: Array[Float], dims: Array[Int]): Tensor =
    torch.Tensor(data).reshape(dims*).native

  def unsafeF64(data: Array[Double], dims: Array[Int]): Tensor =
    torch.Tensor(data).reshape(dims*).native

  def append(lhs: Tensor, rhs: Tensor, axis: Int): Tensor =
    def adapt(t: Tensor): Tensor =
      (axis: @switch) match
        case 0 => if t.dim() == 1 then t.reshape(1, t.shape()(0)) else t
        case 1 => if t.dim() == 1 then t.reshape(t.shape()(0), 1) else t
        case _ => throw IllegalArgumentException("Only tensor up to size 2 are supported")

    println(s"${lhs.shape().toList}, ${rhs.shape().toList}, $axis")
    println(s"${adapt(lhs).shape().toList}, ${adapt(rhs).shape().toList}, $axis")
    global.torch.cat(new TensorArrayRef(new TensorVector(adapt(rhs), adapt(rhs))), axis.toLong)

  def invert(lhs: Tensor): Tensor =
    lhs.inverse()

  def reshape(t: Tensor, shape: Array[Int]): Tensor =
    val dims = shape.map(_.toLong)
    t.reshape(dims*)

  def scalarProduct(lhs: Tensor, rhs: Tensor): Float | Double =
    val scalar = lhs.matmul(rhs.transpose(0, 1))
    scalar.dtype().toScalarType().intern() match
      case org.bytedeco.pytorch.global.torch.ScalarType.Float  => scalar.item_float()
      case org.bytedeco.pytorch.global.torch.ScalarType.Double => scalar.item_double()
      case _                                                   => ???

  def tensorProduct(lhs: Tensor, rhs: Tensor): Tensor =
    lhs.matmul(rhs)

  def transpose(lhs: Tensor): Tensor =
    lhs.transpose(0, 1)

  def floatArray(t: Tensor): Array[Float] =
    torch.Tensor.fromNative[Float32](t).toArray

  def doubleArray(t: Tensor): Array[Float] =
    torch.Tensor.fromNative[Float32](t).toArray
}
