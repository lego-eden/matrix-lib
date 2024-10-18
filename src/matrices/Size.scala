package matrices

import scala.compiletime.ops.int.<

export Size.size

trait Size[T]:
  def size: Int

object Size:
  def size[T](using s: Size[T]): Int = s.size

  given [T <: Int: ValueOf](using 0 < T =:= true): Size[T] = new Size:
    def size: Int = valueOf[T]
end Size
