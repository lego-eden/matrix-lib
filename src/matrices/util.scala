package matrices

import scala.math.Numeric.Implicits.infixNumericOps
import scala.compiletime.ops.int.Min
import scala.compiletime.constValue
import scala.annotation.tailrec

private object util:

  extension [T: Numeric](xs: Vector[T])
    infix def dot(ys: Vector[T]): T =
      xs.zip(ys).map(_ * _).sum

    def sqMagnitude: T =
      xs.map(x => x * x).sum

    infix def parallel(ys: Vector[T]): Boolean =
      val dot = xs dot ys
      dot * dot == xs.sqMagnitude * ys.sqMagnitude

    infix def nonZeroParallel(ys: Vector[T]): Boolean =
      xs.nonZero && ys.nonZero && (xs parallel ys)

    def isZero: Boolean = xs.forall(_ == summon[Numeric[T]].zero)
    def nonZero: Boolean = !xs.isZero

    def *(n: T): Vector[T] = xs.map(_ * n)
    def -(ys: Vector[T]): Vector[T] = xs.zip(ys).map((x, y) => x - y)
  end extension

  extension [T](xs: Vector[T])(using num: Integral[T] | Fractional[T])
    def gcd: T =
      num match
        case int: Integral[T] =>
          given Integral[T] = int
          xs.reduce(util.gcd)
        case frac: Fractional[T] =>
          if xs.forall(_.isWhole(using frac)) then
            given Integral[Int] = scala.math.Numeric.IntIsIntegral
            frac.fromInt(xs.map(_.toInt).gcd)
          else frac.one

  extension [T](xs: Vector[T])(using num: Integral[T] | Fractional[T])
    def simplify: Vector[T] =
      val gcd = xs.gcd
      if gcd != num.one then xs.map(div(_, gcd))
      else xs

  extension [T: Numeric](xss: Vector[Vector[T]])
    def subRegion(row: Int, col: Int): Vector[Vector[T]] =
      xss.zipWithIndex
        .filterNot((_, r) => r == row)
        .map: (rowSeq, _) =>
          rowSeq.zipWithIndex
            .filterNot((_, c) => c == col)
            .map((elem, _) => elem)
    end subRegion

    def colTail: Vector[Vector[T]] =
      xss.transpose.tail.transpose
  end extension

  extension [T](x: T)(using num: Fractional[T])
    def isWhole: Boolean = x == num.fromInt(x.toInt)

  @tailrec
  def gcd[T](a: T, b: T)(using num: Integral[T]): T =
    if b == 0 then a
    else gcd(b, num.rem(a, b))

  def div[T](a: T, b: T)(using num: Integral[T] | Fractional[T]): T =
    num match
      case int: Integral[T]    => int.quot(a, b)
      case frac: Fractional[T] => frac.div(a, b)

  def hasConstantWidth[Tup <: NonEmptyTuple]: Boolean =
    constValue[HasConstantWidth[Tup]]

  type MinLength[Tup <: NonEmptyTuple] <: Int = Tup match
    case x *: xs =>
      xs match
        case NonEmptyTuple => Min[Length[x], MinLength[xs]]
        case _             => Length[x]

  type Length[Type] <: Int = Type match
    case NonEmptyTuple => Tuple.Size[Type]
    case _             => 1

  opaque type Invariant[T] = Nothing

  type HasConstantWidth[Tup <: NonEmptyTuple] <: Boolean =
    Invariant[Tuple.Map[Tup, Length]] match
      case Invariant[Tuple.Map[Tup, [_] =>> Length[Tuple.Head[Tup]]]] => true
      case _                                                          => false

  type NestedUnionType[Tup <: NonEmptyTuple] = MinLength[Tup] match
    case 1 => Tuple.Union[Tup]
    case _ =>
      (Tuple.Head[Tup], Tuple.Tail[Tup]) match
        case (NonEmptyTuple, NonEmptyTuple) =>
          Tuple.Union[Tuple.Head[Tup]] | NestedUnionType[Tuple.Tail[Tup]]
        case (NonEmptyTuple, EmptyTuple) => Tuple.Union[Tuple.Head[Tup]]
        case (t, NonEmptyTuple)          => t | NestedUnionType[Tuple.Tail[Tup]]
        case (t, EmptyTuple)             => t

  infix type ||[F[_], G[_]] = [t] =>> F[t] | G[t]
end util
