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
  def ==(that: DFTypeAny)(using dfc: DFC): Boolean =
    import dfc.getSet
    this.asIR =~ that.asIR
  def !=(that: DFTypeAny)(using dfc: DFC): Boolean =
    import dfc.getSet
    !(this.asIR =~ that.asIR)
  override def toString: String = value.toString
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

  def of[T <: Supported](t: T)(using DFC): Of[T] = DFType(t).asInstanceOf[Of[T]]
  private[core] def apply(t: Any)(using DFC): DFTypeAny =
    t match
      case dfType: DFTypeAny         => dfType
      case tuple: NonEmptyTuple      => DFTuple(tuple)
      case tfe: DFOpaque.Frontend[?] => DFOpaque(tfe)
      case fields: DFStruct.Fields   => DFStruct(fields)
      case _: Byte.type              => DFBits(8)
      case _: Boolean.type           => DFBool
      case _: Int.type               => DFInt32
      case _: Long.type              => DFSInt(64)
      case _: Double.type            => DFDouble
      // TODO: need to add proper upper-bound if fixed in Scalac
      // see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: Object => DFEnum(enumCompanion)
  end apply
  private[core] def unapply(t: Any)(using DFC): Option[DFTypeAny] =
    t match
      case dfVal: DFValAny  => Some(dfVal.dfType)
      case DFTuple(dfType)  => Some(dfType)
      case DFStruct(dfType) => Some(dfType)
      case _                => None

  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = ???
    def codeString(using printer: Printer)(using DFC): String = ???
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  extension (dfType: DFTypeAny) def asFE[T <: DFTypeAny]: T = dfType.asInstanceOf[T]
  transparent inline implicit def conv[T <: Supported](inline t: T)(implicit
      dfc: DFCG,
      tc: TC[T]
  ): DFTypeAny = tc(t)
  export DFDecimal.Extensions.*
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
  export DFEnum.given
  export DFVector.given
  export TDFDouble.given
  export TDFString.given

  given [T <: DFTypeAny]: CanEqual[T, T] = CanEqual.derived

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
    def apply[T, OT <: DFTypeAny](value: OT): Aux[T, OT] =
      new TC[T]:
        type Type = OT
        def apply(t: T)(using DFC): Type = value

    given ofDFType[T <: DFTypeAny]: TC[T] with
      type Type = T
      def apply(t: T)(using DFC): Type = t

    given ofBooleanCompanion: TC[Boolean.type] with
      type Type = DFBool
      def apply(t: Boolean.type)(using DFC): Type = DFBool

    given ofByteCompanion: TC[Byte.type] with
      type Type = DFBits[8]
      def apply(t: Byte.type)(using DFC): Type = DFBits(8)

    given ofIntCompanion: TC[Int.type] with
      type Type = DFInt32
      def apply(t: Int.type)(using DFC): Type = DFInt32

    given ofDoubleCompanion: TC[Double.type] with
      type Type = DFDouble
      def apply(t: Double.type)(using DFC): Type = DFDouble

    given ofLongCompanion: TC[Long.type] with
      type Type = DFSInt[64]
      def apply(t: Long.type)(using DFC): Type = DFSInt(64)

    given ofOpaque[T <: DFTypeAny, TFE <: DFOpaque.Frontend[T]]: TC[TFE] with
      type Type = DFOpaque[TFE]
      def apply(t: TFE)(using DFC): Type = DFOpaque(t)

    transparent inline given ofProductCompanion[T <: Object]: TC[T] = ${ productMacro[T] }
    def productMacro[T <: Object](using Quotes, Type[T]): Expr[TC[T]] = ???

    transparent inline given ofTuple[T <: NonEmptyTuple]: TC[T] = ${ ofTupleMacro[T] }
    def ofTupleMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[TC[T]] = ???

    transparent inline given ofNamedTuple[N <: NonEmptyTuple, T <: NonEmptyTuple]
        : TC[NamedTuple[N, T]] = ${ ofNamedTupleMacro[N, T] }
    def ofNamedTupleMacro[N <: NonEmptyTuple, T <: NonEmptyTuple](using
        Quotes, Type[N], Type[T]
    ): Expr[TC[NamedTuple[N, T]]] = ???
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
