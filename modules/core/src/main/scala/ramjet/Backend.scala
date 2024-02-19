package ramjet

/* A Backend is a low level implementation of tensor operations
 * The Tensor type parameters carries all the context and is responsible for implementing all optimizations
 * The Scalar type represent the underlying field for computations
 */
trait Backend[Scalar, Tensor] {

  /* Unsafe way to create a Tensor by copying an array of the underlying scala field
   * `dims` represent the tensor shape
   * TODO distinguish between the copy and view field
   */
  def unsafe(data: Array[Scalar], dims: Array[Int]): Tensor

  def scalarProduct(lhs: Tensor, rhs: Tensor): Scalar

  /* Tensor product implementation. The implementor MUST match on shapes to handle all the case he desire to handle */
  def tensorProduct(lhs: Tensor, rhs: Tensor): Tensor

  def transpose(lhs: Tensor): Tensor

  final val api: TensorAPI[Scalar, Tensor] = TensorAPI(this)
}
