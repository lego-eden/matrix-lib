package matrices

import scala.compiletime.ops.int.*
import scala.annotation.implicitNotFound

@implicitNotFound("The size must be 1 or larger")
type Size[I <: Int] = 0 < I =:= true
