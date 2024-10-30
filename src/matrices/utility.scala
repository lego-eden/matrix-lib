package matrices

import scala.math.Numeric.Implicits.infixNumericOps
import scala.compiletime.ops.int.Min
import scala.compiletime.constValue

extension [T: Numeric](xs: Vector[T])
  private infix def dot(ys: Vector[T]): T =
    xs.zip(ys).map(_ * _).sum

  private def sqMagnitude: T =
    xs.map(x => x * x).sum

  private infix def parallel(ys: Vector[T]): Boolean =
    val dot = xs dot ys
    dot * dot == xs.sqMagnitude * ys.sqMagnitude

  private infix def nonZeroParallel(ys: Vector[T]): Boolean =
    xs.nonZero && ys.nonZero && (xs parallel ys)

  private def isZero: Boolean = xs.forall(_ == summon[Numeric[T]].zero)
  private def nonZero: Boolean = !xs.isZero

  private def *(n: T): Vector[T] = xs.map(_ * n)
  private def -(ys: Vector[T]): Vector[T] = xs.zip(ys).map((x, y) => x - y)

extension [T: Numeric](xss: Vector[Vector[T]])
  private def subRegion(row: Int, col: Int): Vector[Vector[T]] =
    xss.zipWithIndex
      .filterNot((_, r) => r == row)
      .map: (rowSeq, _) =>
        rowSeq.zipWithIndex
          .filterNot((_, c) => c == col)
          .map((elem, _) => elem)
  end subRegion

private type MinLength[Tup <: NonEmptyTuple] <: Int = Tup match
  case x *: xs =>
    xs match
      case NonEmptyTuple => Min[Length[x], MinLength[xs]]
      case _             => Length[x]

private type Length[Type] <: Int = Type match
  case NonEmptyTuple => Tuple.Size[Type]
  case _             => 1

private opaque type Invariant[T] = Nothing

private type HasConstantWidth[Tup <: NonEmptyTuple] <: Boolean =
  Invariant[Tuple.Map[Tup, Length]] match
    case Invariant[Tuple.Map[Tup, [_] =>> Length[Tuple.Head[Tup]]]] => true
    case _                                                          => false

private def hasConstantWidth[Tup <: NonEmptyTuple]: Boolean =
  constValue[HasConstantWidth[Tup]]

private type NestedUnionType[Tup <: NonEmptyTuple] = MinLength[Tup] match
  case 1 => Tuple.Union[Tup]
  case _ =>
    (Tuple.Head[Tup], Tuple.Tail[Tup]) match
      case (NonEmptyTuple, NonEmptyTuple) =>
        Tuple.Union[Tuple.Head[Tup]] | NestedUnionType[Tuple.Tail[Tup]]
      case (NonEmptyTuple, EmptyTuple) => Tuple.Union[Tuple.Head[Tup]]
      case (t, NonEmptyTuple)          => t | NestedUnionType[Tuple.Tail[Tup]]
      case (t, EmptyTuple)             => t
