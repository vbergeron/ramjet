package ramjet

trait Backend {
  type Scalar
  type Tensor

  def unsafe(data: Array[Scalar]): Tensor
  def scalarProduct(lhs: Tensor, rhs: Tensor): Scalar
  def tensorProduct(lhs: Tensor, rhs: Tensor): Tensor
}
