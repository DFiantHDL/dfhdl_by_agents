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
  type Of[T <: Supported] <: DFTypeAny = T match
    case DFTypeAny => T & DFTypeAny
    case Int       => DFInt32
    case Long      => DFSInt[64]
    case Byte      => DFBits[8]
    case Boolean   => DFBool
    case Double    => DFDouble
    case DFOpaqueA => DFOpaque[T]
    case String    => DFString
    case Product   => FromProduct[T]
    case Unit      => DFUnit

  type FromProduct[T <: Product] <: DFTypeAny = T match
    case DFEncoding      => DFEnum[T]
    case NonEmptyTuple   => DFTuple[Tuple.Map[T, JUSTVAL]]
    case DFStruct.Fields => DFStruct[T]

  type FromDFVal[T] <: DFTypeAny = T match
    case DFVal[t, ?] => t

  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = ???
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
  extension (dfType: DFTypeAny) def asFE[T <: DFTypeAny]: T = ???
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
  export DFEnum.given

  type Supported = DFTypeAny | FieldsOrTuple | DFEncoding | DFOpaqueA | Byte | Int | Long |
    Boolean | Double | String | Object | Unit

  trait TC[T]:
    type Type <: DFTypeAny
    def apply(t: T)(using DFC): Type
  trait TCLP:
    transparent inline given errorDMZ[T](using t: ShowType[T]): TC[T] =
      Error.call[
        (
            "Dataflow type cannot be constructed from the type `",
            t.Out,
            "`."
        )
      ]
  object TC extends TCLP:
    type Aux[T, OT <: DFTypeAny] = TC[T] { type Type = OT }
    given ofDFType[T <: DFTypeAny]: TC[T] with
      type Type = T
      def apply(t: T)(using DFC): Type = ???
  end TC

  extension [LW <: IntP](lhs: DFTypeW[LW])
    protected[core] def compareWidths[RW <: IntP](rhs: DFTypeW[RW])(
        func: (Int, Int) => Boolean
    )(using dfc: DFC): Option[Boolean] = ???
    protected[core] def widthCodeString(using dfc: DFC): String = ???

end DFType

type DFTypeW[W <: IntP] = DFBits[W] | DFUInt[W] | DFSInt[W]

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tcDFType")
  def dfType(using DFC): tc.Type = tc(t)

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValDFType")
  def dfType: T = dfVal.asIR.dfType.asFE[T]

extension (intParamRef: ir.IntParamRef)
  def dropUnreachableRef(allowDesignParamRefs: Boolean)(using dfc: DFC): ir.IntParamRef = ???
end extension

extension (dfType: ir.DFType)
  def dropUnreachableRefs(allowDesignParamRefs: Boolean)(using dfc: DFC): ir.DFType = ???
  def dropUnreachableRefs(using DFC): ir.DFType = ???
end extension
