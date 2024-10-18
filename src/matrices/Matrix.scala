package matrices

import scala.compiletime.ops.int.<
import scala.math.Numeric.Implicits.infixNumericOps

export Size.size

trait Size[T]:
  def size: Int

object Size:
  def size[T](using s: Size[T]): Int = s.size

  given [T <: Int: ValueOf](using 0 < T =:= true): Size[T] = new Size:
    def size: Int = valueOf[T]
end Size

extension [T: Numeric](xs: Vector[T])
  infix def dot(ys: Vector[T]): T =
    xs.zip(ys).map(_ * _).sum

case class Matrix[H: Size, W: Size, T: Numeric] private (
    rows: Vector[Vector[T]]
):
  lazy val cols = rows.transpose
  lazy val height = rows.length
  lazy val width = cols.length

  def transpose: Matrix[W, H, T] = new Matrix(cols) // cols are transposed

  def *[W2: Size](other: Matrix[W, W2, T]): Matrix[H, W2, T] =
    new Matrix(
      Vector.tabulate(height, other.width)(rows(_) dot other.cols(_))
    )
  
  def +(other: Matrix[H, W, T]): Matrix[H, W, T] =
    new Matrix(
      rows.zip(other.rows).map: (row1, row2) =>
        row1.zip(row2).map(_ + _)
    )

  override def toString: String =
    "\n" +
      rows
        .map(row => s"| ${row.mkString(" ")} |")
        .mkString("\n")
  end toString

end Matrix

object Matrix:
  /** Create a new [[matrices.Matrix]] with the specified dimensions filled with
    * zeroes.
    *
    * @tparam H
    *   the height of the matrix
    * @tparam W
    *   the width of the matrix
    * @tparam T
    *   the numeric type of the matrix
    *
    * @return
    *   the matrix
    */
  def apply[H: Size, W: Size, T](using num: Numeric[T]): Matrix[W, H, T] =
    new Matrix(
      Vector.fill(size[H], size[W])(num.zero)
    )

  /**
    * Create a new [[matrices.Matrix]] with the specified dimensions by tabulating
    * using the specified function.
    *
    * @tparam H
    *   the height of the matrix
    * @tparam W
    *   the width of the matrix
    * @tparam T
    *   the numeric type of the matrix
    * 
    * @param func
    *   the function to use for tabulating
    * @return
    *   the matrix
    */
  def tabulate[H <: Int: Size, W <: Int: Size, T: Numeric](
      func: (Int, Int) => T
  ): Matrix[H, W, T] =
    new Matrix(
      Vector.tabulate(size[H], size[W])(func)
    )

  /**
    * The identity matrix of the specified size.
    *
    * @tparam S
    *   the size of the matrix
    * @tparam T
    *   the numeric type of the matrix
    * 
    * @return
    *   the matrix
    */
  def identity[S <: Int: Size, T](using num: Numeric[T]): Matrix[S, S, T] =
    new Matrix(
      Vector.tabulate(size[S], size[S]): (row, col) =>
        if row == col then num.one else num.zero
    )
