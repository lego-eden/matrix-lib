package matrices

import scala.math.Numeric.Implicits.infixNumericOps
import scala.annotation.targetName

/** A generic, type-safe, mathematical matrix whose elements can be of any
  * numeric type.
  *
  * @param rows
  *   the rows of the matrix
  */
case class Matrix[H: Size, W: Size, T: Numeric] private (
    rows: Vector[Vector[T]]
):
  /** the columns of the matrix */
  lazy val cols = rows.transpose

  /** the height of the matrix */
  lazy val height = size[H]

  /** the width of the matrix */
  lazy val width = size[W]

  /** Return the element at the specified row and column.
    *
    * @param row
    *   the row of the element
    * @param col
    *   the column of the element
    * @return
    *   the element at (row, col)
    */
  def apply(row: Int, col: Int): T = rows(row)(col)

  /** Set (row, col) to the specified element.
    *
    * @param row
    *   the row to update
    * @param col
    *   the column to update
    * @param elem
    *   the new element
    * @return
    *   the updated matrix
    */
  def set(row: Int, col: Int, elem: T): Matrix[H, W, T] =
    new Matrix(
      rows.updated(row, rows(row).updated(col, elem))
    )

  /** A version of set with multiple parameter lists, allowing for partial
    * application and syntactic sugar.
    *
    * @param row
    *   the row to update
    * @param col
    *   the column to update
    * @param elem
    *   the new element
    * @return
    *   the updated matrix
    */
  @targetName("set2")
  def set(row: Int, col: Int)(elem: T): Matrix[H, W, T] = set(row, col, elem)

  /** The transposed version of this matrix.
    *
    * @return
    *   the transposed matrix
    */
  def transpose: Matrix[W, H, T] = new Matrix(cols) // cols are transposed

  /** Multiply this matrix with another matrix. The resulting matrix has the
    * height of this one and the width of the other one.
    *
    * @param other
    *   the matrix to multiply this one with
    * @return
    *   the resulting matrix of the multiplication
    */
  def *[W2: Size](other: Matrix[W, W2, T]): Matrix[H, W2, T] =
    new Matrix(
      Vector.tabulate(height, other.width)(rows(_) dot other.cols(_))
    )

  /** Multiply this matrix with a scalar, returning a new matrix where the
    * elements are scaled by the provided scalar.
    *
    * @param scalar
    *   the value to scale each element by
    * @return
    *   the scaled matrix
    */
  def *(scalar: T): Matrix[H, W, T] =
    new Matrix(
      rows.map(row => row.map(_ * scalar))
    )

  /** Add this matrix to another one and return the result.
    *
    * @param other
    *   the matrix to sum with this one.
    * @return
    *   the matrix sum
    */
  def +(other: Matrix[H, W, T]): Matrix[H, W, T] =
    new Matrix(
      rows
        .zip(other.rows)
        .map: (row1, row2) =>
          row1.zip(row2).map(_ + _)
    )

  /** Subtract a matrix from this one element-wise. This is equivalant to adding
    * the negation of the other matrix to this one.
    *
    * @param other
    *   the matrix to subtract from this one.
    * @return
    *   the resulting matrix of the subtraction.
    */
  def -(other: Matrix[H, W, T])(using num: Numeric[T]): Matrix[H, W, T] =
    this + -this

  def unary_-(using num: Numeric[T]): Matrix[H, W, T] = this * -num.one

  /** Takes a function which takes 3 arguments and returns a new numeric type U.
    * This function is applied on each element of the matrix, returning a new,
    * transformed, matrix.
    *
    * @param func
    *   the function to apply on each element on the form `(oldValue, row, col)
    *   \=> newValue`
    * @return
    *   the matrix with the function mapped on each element
    */
  def mapWithIndex[U: Numeric](func: (T, Int, Int) => U): Matrix[H, W, U] =
    new Matrix(
      rows.zipWithIndex.map: (rowSeq, row) =>
        rowSeq.zipWithIndex.map: (elem, col) =>
          func(elem, row, col)
    )

  def map[U: Numeric](func: T => U): Matrix[H, W, U] =
    new Matrix(
      rows.map: rowSeq =>
        rowSeq.map: elem =>
          func(elem)
    )

  override def toString: String =
    "\n" +
      rows
        .map(row => s"| ${row.mkString(" ")} |")
        .mkString("\n")
  end toString

end Matrix

/** Companion object to the [[Matrix]] class. This object contains several
  * utility-methods for creating matrices.
  */
object Matrix:
  /** Create a new [[Matrix]] with the specified dimensions filled with zeroes.
    *
    * @tparam T
    *   the numeric type of the matrix
    * @param height
    *   the height of the matrix
    * @param width
    *   the width of the matrix
    *
    * @return
    *   the matrix
    */
  def zero[T](height: Int, width: Int)(using
      _h: Size[height.type],
      _w: Size[width.type],
      num: Numeric[T]
  ): Matrix[height.type, width.type, T] =
    new Matrix(
      Vector.fill(height, width)(num.zero)
    )

  /** Create a new square [[Matrix]] with the specified size filled with zeroes.
    *
    * @tparam T
    *   the numeric type of the matrix
    *
    * @param size
    *   the width and height of the matrix
    *
    * @return
    *   the matrix
    */
  def zero[T: Numeric](size: Int)(using
      Size[size.type]
  ): Matrix[size.type, size.type, T] = zero[T](size, size)

  /** Create a new [[Matrix]] with the specified dimensions by tabulating using
    * the specified function.
    *
    * @tparam T
    *   the numeric type of the matrix
    * @param height
    *   the height of the matrix
    * @param width
    *   the width of the matrix
    *
    * @param func
    *   the function to use for tabulating
    * @return
    *   the matrix
    */
  def tabulate[T: Numeric](
      height: Int,
      width: Int
  )(
      func: (Int, Int) => T
  )(using
      _h: Size[height.type],
      _w: Size[width.type]
  ): Matrix[height.type, width.type, T] =
    new Matrix(
      Vector.tabulate(height, width)(func)
    )

  /** The identity matrix of the specified size.
    *
    * @tparam T
    *   the numeric type of the matrix
    * @param size
    *   the size of the matrix
    *
    * @return
    *   the matrix
    */
  def identity[T: Numeric](size: Int)(using
      _s: Size[size.type]
  ): Matrix[size.type, size.type, T] =
    identity[size.type, T]

  private def identity[S: Size, T](using num: Numeric[T]): Matrix[S, S, T] =
    new Matrix(
      Vector.tabulate(size[S], size[S]): (row, col) =>
        if row == col then num.one else num.zero
    )

  /** Extension methods exclusive to square matrices */
  extension [S: Size, T: Numeric](mat: Matrix[S, S, T])
    infix def pow(n: Int): Matrix[S, S, T] =
      (0 until n).foldLeft(Matrix.identity[S, T])((acc, _) => acc * mat)
  
  extension [T: Numeric](mat: Matrix[1, 1, T])
    /** trivial determinant when the matrix is 1x1 */
    @targetName("determinant1")
    def determinant: T = mat(0, 0)
  
  extension [T](using num: Numeric[T])(mat: Matrix[2, 2, T])
    @targetName("determinant2")
    def determinant: T =
      val a: T = mat(0, 0)
      val b: T = mat(0, 1)
      val c: T = mat(1, 0)
      val d: T = mat(1, 1)

      (num.times(a, d)) - (num.times(b, c))
    end determinant

  extension [T: Numeric](x: T)
    def *[H, W](mat: Matrix[H, W, T]): Matrix[H, W, T] = mat * x
  
end Matrix
