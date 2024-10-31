package matrices

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.targetName
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
import scala.util.NotGiven
import scala.annotation.implicitNotFound

import util.*

/** A generic, type-safe, matrix.
  *
  * @param rows
  *   the rows of the matrix
  */
case class Matrix[H <: Int: Size, W <: Int: Size, T] private (
    rows: Vector[Vector[T]]
):
  /** the columns of the matrix */
  lazy val cols = rows.transpose

  /** the height of the matrix */
  lazy val height = rows.length

  /** the width of the matrix */
  lazy val width = rows(0).length

  lazy val diagonals: Vector[T] =
    0.until(width min height)
      .map(i => apply(i, i))
      .toVector

  def rank(using Integral[T] | Fractional[T]): Int =
    ref.rows.filterNot(_.isZero).size

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
  def set(row: Int, col: Int, elem: => T): Matrix[H, W, T] =
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
  def set(row: Int, col: Int)(elem: => T): Matrix[H, W, T] = set(row, col, elem)

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
  def *[W2 <: Int: Size](other: Matrix[W, W2, T])(using
      Numeric[T]
  ): Matrix[H, W2, T] =
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
  def *(scalar: T)(using Numeric[T]): Matrix[H, W, T] =
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
  def +(other: Matrix[H, W, T])(using Numeric[T]): Matrix[H, W, T] =
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
    this + -other

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

  def subMatrix(row: Int, col: Int)(using
      @implicitNotFound(
        "The matrix must be at least 2 high to create a sub-matrix"
      ) h: Size[H - 1],
      @implicitNotFound(
        "The matrix must be at least 2 wide to create a sub-matrix"
      ) w: Size[W - 1],
      num: Numeric[T]
  ): Matrix[H - 1, W - 1, T] =
    new Matrix(
      rows.subRegion(row, col)
    )

  private def refWithTransform(using
      num: Integral[T] | Fractional[T]
  ): (Matrix[H, W, T], T, T) =
    // updates when swapping rows or multiplying row by constant
    var detFactor: T = num.one
    var detDivisor: T = num.one

    def ref(rows: Vector[Vector[T]]): Vector[Vector[T]] =
      def recurse(rows: Vector[Vector[T]]): Vector[Vector[T]] =
        if rows.isEmpty then rows
        else
          (rows.transpose.head +: // the first column
            ref(rows.colTail).transpose // the reduced columns.tail
          ).transpose // convert columns to rows

      val nonZeroRows = rows.indices.filterNot(rows(_)(0) == num.zero)

      if rows.isEmpty then rows // base case reached
      else if nonZeroRows.isEmpty then
        // the first column contains only zeroes. Discard it and recurse
        recurse(rows)
      else
        val cols = rows.transpose
        val minRow: Int = nonZeroRows.minBy(rows(_)(0).abs)

        val pivotRow: Vector[T] = rows(minRow) * rows(minRow)(0).sign
        if rows(minRow)(0).sign == -num.one then detFactor = -detFactor
        val pivot: T = pivotRow(0)
        val reducedRows = nonZeroRows
          .filter(r => r != minRow)
          .foldLeft(rows): (acc, r) =>
            val scaledRow = rows(r) * pivot
            detFactor *= pivot // row was multiplied by a constant
            val reducedRow = scaledRow - (pivotRow * rows(r)(0))
            acc.updated(r, reducedRow)
        val newRows = reducedRows // swap first- and pivot-row
          .updated(minRow, reducedRows(0))
          .updated(0, pivotRow)
        if minRow != 0 then
          detFactor = -detFactor // only update the detfactor if a swap happened
        detDivisor *= newRows.head.gcd
        newRows.head.simplify +: recurse(newRows.tail)
      end if
    end ref

    (new Matrix(ref(rows)), detFactor, detDivisor)
  end refWithTransform

  /** Returns the row echelon form of this matrix.
    *
    * @return
    *   the row echelon form of the matrix
    */
  def ref(using Integral[T] | Fractional[T]): Matrix[H, W, T] =
    refWithTransform._1

  /** Returns the reduced row echelon form of this matrix.
    *
    * @return
    *   the reduced row echelon form of the matrix
    */
  def rref(using Integral[T] | Fractional[T]): Matrix[H, W, T] =
    // the "rows" parameter is in reduced form
    def rref(rows: Vector[Vector[T]]): Vector[Vector[T]] =
      ???

    end rref

    new Matrix(rref(ref.rows))
  end rref

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
      h: Size[height.type],
      w: Size[width.type],
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
      s: Size[size.type]
  ): Matrix[size.type, size.type, T] = zero[T](size, size)

  def apply(
      rows: NonEmptyTuple
  )(using
      widthValue: ValueOf[MinLength[rows.type]],
      heightSize: Size[Tuple.Size[rows.type]],
      widthSize: Size[MinLength[rows.type]]
  ): Matrix[Tuple.Size[rows.type], MinLength[rows.type], NestedUnionType[
    rows.type
  ]] =
    val height: Tuple.Size[rows.type] = rows.size
    val width: MinLength[rows.type] = widthValue.value

    val elementFunc: (Int, Int) => NestedUnionType[rows.type] =
      if width > 1 && hasConstantWidth[rows.type] then
        (r, c) =>
          // the "width" of the tuple is greater than 1 =>
          //    safe to cast any element of it as a tuple
          rows(r)
            .asInstanceOf[NonEmptyTuple](c)
            .asInstanceOf[NestedUnionType[rows.type]]
      else (r, c) => rows(r).asInstanceOf[NestedUnionType[rows.type]]

    new Matrix(
      Vector.tabulate(height, width)(elementFunc)
    )
  end apply

  /** Create a new [[Matrix]] of the specified dimensions filled with a provided
    * value.
    *
    * @param height
    *   the number rows in the matrix
    * @param width
    *   the number of columns in the matrix
    * @param elem
    *   the element to fill the matrix with
    * @return
    *   the matrix
    */
  def fill[T](height: Int, width: Int)(elem: => T)(using
      h: Size[height.type],
      w: Size[width.type]
  ): Matrix[height.type, width.type, T] =
    new Matrix(
      Vector.fill(height, width)(elem)
    )

  /** Create a new square [[Matrix]] with the specified size filled with the
    * provided element.
    *
    * @param size
    *   the number of rows and columns in the matrix
    * @param elem
    *   the element to fill the matrix with
    * @return
    *   the newly created matrix
    */
  def fill[T](size: Int)(elem: => T)(using
      s: Size[size.type]
  ): Matrix[size.type, size.type, T] = fill(size, size)(elem)

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
  def tabulate[T](
      height: Int,
      width: Int
  )(
      func: (Int, Int) => T
  )(using
      h: Size[height.type],
      w: Size[width.type]
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
      s: Size[size.type],
      sizeVal: ValueOf[size.type]
  ): Matrix[size.type, size.type, T] =
    identity[size.type, T]

  private def identity[S <: Int: Size, T](using
      num: Numeric[T],
      sizeVal: ValueOf[S]
  ): Matrix[S, S, T] =
    new Matrix(
      Vector.tabulate(sizeVal.value, sizeVal.value): (row, col) =>
        if row == col then num.one else num.zero
    )

  /** Extension methods exclusive to square matrices */
  extension [S <: Int: Size: ValueOf, T](mat: Matrix[S, S, T])
    infix def pow(n: Int)(using Numeric[T]): Matrix[S, S, T] =
      (0 until n).foldLeft(Matrix.identity[S, T])((acc, _) => acc * mat)

    /** Compute and return the determinant of this matrix.
      *
      * NOTE: The return value of this method may be wrong if an integer
      * overflow occurs. To avoid this, convert the matrix elements to a type
      * which does not have overflow such as BigInt, and then convert back.
      *
      * ```
      * val mat = Matrix.fill(5, 5)(util.Random.nextInt(10))
      * val maybeDet = mat.determinant
      * val guaranteedDet = mat.map(BigInt(_)).determinant.toInt
      * ```
      *
      * @return
      *   the determinant
      */
    def determinant(using Integral[T] | Fractional[T]): T =
      val (reduced, factor, divisor) = mat.refWithTransform
      val det = reduced.diagonals.product
      div(det * divisor, factor)
    end determinant

    /** Compute and return the determinant of this matrix. Alias for
      * [[Matrix.determinant]].
      *
      * @return
      *   the determinant
      */
    def det(using Integral[T] | Fractional[T]): T =
      mat.determinant
  end extension
end Matrix

object numericExtensions:
  extension [T: Numeric](x: T)
    def *[H <: Int, W <: Int](mat: Matrix[H, W, T]): Matrix[H, W, T] = mat * x
