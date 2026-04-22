package dfhdl.core
import dfhdl.compiler.ir
import scala.annotation.targetName

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args
sealed trait Args4[T1, T2, T3, T4] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T | DFError) extends AnyVal
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given

  type Supported = Any
  type Of[T <: Supported] = DFTypeAny

  trait TC[T]:
    type Type <: DFTypeAny
  object TC:
    type Aux[T, OT <: DFTypeAny] = TC[T] { type Type = OT }
end DFType

