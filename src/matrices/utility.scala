package matrices

import scala.math.Numeric.Implicits.infixNumericOps

extension [T: Numeric](xs: Vector[T])
  private infix def dot(ys: Vector[T]): T =
    xs.zip(ys).map(_ * _).sum
