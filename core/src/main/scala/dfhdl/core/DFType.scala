package dfhdl.core
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap
import DFOpaque.Abstract as DFOpaqueA
import NamedTuple.{NamedTuple, AnyNamedTuple}
import scala.annotation.implicitNotFound

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args
sealed trait Args4[T1, T2, T3, T4] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T | DFError) extends AnyVal:
  def ==(that: DFTypeAny)(using dfc: DFC): Boolean = ???
  def !=(that: DFTypeAny)(using dfc: DFC): Boolean = ???
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = ???
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
  extension (dfType: DFTypeAny) def asFE[T <: DFTypeAny]: T = ???
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given

  type Supported = Any
  type Of[T <: Supported] = DFTypeAny

  trait TC[T]:
    type Type <: DFTypeAny
  object TC
end DFType

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValDFType")
  def dfType: T = ???
