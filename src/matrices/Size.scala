package matrices

import scala.compiletime.ops.int.*
import scala.annotation.implicitNotFound

export Size.size

sealed trait Size[T]:
  def size: Int

object Size:
  def size[T](using s: Size[T]): Int = s.size

  given [T <: Int: ValueOf](using 0 < T =:= true): Size[T] = new Size:
    def size: Int = valueOf[T]

  given [T <: Int: Size](using 1 < T =:= true): Size[T - 1] = new Size[T - 1]:
    def size: Int = Size.size[T] - 1
end Size
