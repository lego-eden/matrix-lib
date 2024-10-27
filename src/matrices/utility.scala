package matrices

import scala.math.Numeric.Implicits.infixNumericOps
import scala.compiletime.ops.int.Min

extension [T: Numeric](xs: Vector[T])
  private infix def dot(ys: Vector[T]): T =
    xs.zip(ys).map(_ * _).sum

  private def sqMagnitude: T =
    xs.map(x => x * x).sum

  private infix def parallel(ys: Vector[T]): Boolean =
    val dot = xs dot ys
    dot * dot == xs.sqMagnitude * ys.sqMagnitude

  private infix def nonZeroParallel(ys: Vector[T]): Boolean =
    (xs.nonZero && ys.nonZero) && (xs parallel ys)

  private def isZero: Boolean = xs.forall(_ == summon[Numeric[T]].zero)
  private def nonZero: Boolean = !xs.isZero

extension [T: Numeric](xss: Vector[Vector[T]])
  private def subRegion(row: Int, col: Int): Vector[Vector[T]] =
    xss.zipWithIndex
      .filterNot((_, r) => r == row)
      .map: (rowSeq, _) =>
        rowSeq.zipWithIndex
          .filterNot((_, c) => c == col)
          .map((elem, _) => elem)
  end subRegion

private type Width[X <: NonEmptyTuple] = Length[Tuple.Head[X]]

private type Length[X] <: Int = X match
  case NonEmptyTuple => Tuple.Size[X]
  case _ => 1

private type Square[X <: NonEmptyTuple] =
  Tuple.Map[X, Length] =:= Tuple.Map[X, [x] =>> Length[Tuple.Head[X]]]