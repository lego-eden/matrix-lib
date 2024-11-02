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
  * NOTE: The numeric operations on this matrix may error due to overflow. Keep
  * this in mind when using a matrix of integers. Use a matrix of BigInt if you
  * want guaranteed correct results when using integers.
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
      num: Integral[T] | Fractional[T],
      hSize: Size[H],
      wSize: Size[W]
  ): Vector[Vector[T]] =
    // idé:
    // loopa igenom alla rader och ersätt varje rad med
    // ((rows(i) * rows(k)(k) - rows(k) * rows(i)(k)) / rows(k - 1)(k - 1))
    // där k är indexet för pivotraden, och i den nuvarande raden under pivotraden.
    // Notera att för första pivotraden, alltså k = 0, rows(k - 1)(k - 1) att ge fel.
    // Det borde ge 1.
    var offset = 0 // this updates when a row contains only zeroes
    (0 until ((height min width) - 1)).foldLeft(rows): (ref, pivot) =>
      ref.indices.drop(pivot - offset).find(r => ref(r)(pivot) != 0) match
        case None =>
          offset += 1
          ref
        case Some(row) =>
          val fixedRows =
            val fixedRows = ref.swap(row, pivot - offset)
            if row != (pivot - offset) then
              fixedRows.updated(fixedRows.indices.last, -fixedRows.last)
            else fixedRows
          fixedRows.indices
            .drop(pivot + 1 - offset)
            .foldLeft(fixedRows): (acc, r) =>
              acc.updated(
                r,
                ((acc(r) * acc(pivot - offset)(pivot)) -
                  (acc(pivot - offset) * acc(r)(pivot))) / acc
                  .applyOrElse(pivot - 1 - offset, _ => Vector())
                  .applyOrElse(pivot - 1, _ => num.one)
              )
  end refWithTransform

  /** Returns the row echelon form of this matrix.
    *
    * @return
    *   the row echelon form of the matrix
    */
  def ref(using Integral[T] | Fractional[T]): Matrix[H, W, T] =
    // new Matrix(refWithTransform.map(_.simplify))
    new Matrix(refWithTransform)

  /** Returns the hermite normal form of this matrix.
    *
    * NOTE: If the matrix does not contain integers the result of this method is
    * not the hermite normal form, nor is it the row reduced normal form. Make
    * of that what you will. This method is still available for all matrices
    * since that pseudo-hermite-normal-form may be useful.
    *
    * @return
    *   the hermite normal form of the matrix
    */
  def hnf(using Integral[T] | Fractional[T]): Matrix[H, W, T] =
    // the "rows" parameter is in reduced form
    def hnf(rows: Vector[Vector[T]]): Vector[Vector[T]] =
      // start with the lowest pivot row and work upwards
      val pivotIndices = rows.filter(_.nonZero).indices
      pivotIndices.reverse.foldLeft(rows): (rref, c) =>
        val pivotRow = rref(c).simplify
        val pivot =
          pivotRow.pivot.get // cannot be zero since we filtered out all non-zero rows
        pivotIndices
          .take(c)
          .foldLeft(rref): (acc, r) =>
            val reducedRow = (acc(r) * pivot) - (pivotRow * acc(r)(c))
            acc.updated(c, pivotRow).updated(r, reducedRow.simplify)
    end hnf

    new Matrix(hnf(ref.rows))
  end hnf

  def rref(using Fractional[T]): Matrix[H, W, T] =
    new Matrix(
      hnf.rows.indices
        .filter(r => rows(r).nonZero)
        .map(r => rows(r) / rows(r).pivot.get)
        .toVector
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
      * @return
      *   the determinant
      */
    def determinant(using Integral[T] | Fractional[T]): T =
      val ref = mat.refWithTransform
      ref.last.last
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
