package ramjet

trait Backend[Scalar, Tensor] {

  def unsafe(data: Array[Scalar], dims: Array[Int]): Tensor
  def scalarProduct(lhs: Tensor, rhs: Tensor): Scalar
  def tensorProduct(lhs: Tensor, rhs: Tensor): Tensor

  final inline def api: TensorAPI[Scalar, Tensor] = TensorAPI(this)
}
