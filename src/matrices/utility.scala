package matrices

import scala.math.Numeric.Implicits.infixNumericOps

extension [T: Numeric](xs: Vector[T])
  private infix def dot(ys: Vector[T]): T =
    xs.zip(ys).map(_ * _).sum

extension [T: Numeric](xss: Vector[Vector[T]])
  private def subRegion(row: Int, col: Int): Vector[Vector[T]] =
    xss.zipWithIndex
      .filterNot((_, r) => r == row)
      .map: (rowSeq, _) =>
        rowSeq.zipWithIndex
          .filterNot((_, c) => c == col)
          .map((elem, _) => elem)
  end subRegion
