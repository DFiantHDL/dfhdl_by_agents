package dfhdl.core
import scala.annotation.Annotation

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  trait DFType
  trait DFBits extends DFType
  trait DFBoolOrBit extends DFType
  trait DFBool extends DFType
  object DFBool extends DFBool
  trait DFDecimal extends DFType
  trait DFVal
  trait Meta
  val DFInt32: DFType = ???
  object DFVal:
    object Func:
      sealed trait Op
      object Op:
        sealed trait `+` extends Op; object `+` extends `+`
        sealed trait `-` extends Op; object `-` extends `-`
        sealed trait `*` extends Op; object `*` extends `*`
        sealed trait `/` extends Op; object `/` extends `/`
        sealed trait `%` extends Op; object `%` extends `%`
        sealed trait max extends Op; object max extends max
        sealed trait min extends Op; object min extends min
        sealed trait `|` extends Op; object `|` extends `|`
        sealed trait `&` extends Op; object `&` extends `&`
        sealed trait `^` extends Op; object `^` extends `^`
  object DFDecimal:
    sealed trait NativeType
    object NativeType:
      sealed trait Int32 extends NativeType
      sealed trait BitAccurate extends NativeType
end ir

trait Position
trait MetaContext
trait ExactOp2Aux[Op, C, B, L, R, O]

sealed class Modifier[+A, +C, +I, +P]
type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  type CONST = Modifier[Any, Any, Any, Any]

type IntP = Any
object IntP:
  type +[L <: IntP, R <: IntP] = Int
  type Max[L <: IntP, R <: IntP] = Int
end IntP

export DFType.asFE

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args4[T1, T2, T3, T4] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T) extends AnyVal
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
end DFType

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  given [W <: IntP & Singleton]: DFBits[W] = ???
end DFBits

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    mutableDB: MutableDB = new MutableDB()
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt,
      annotations: List[Annotation] = Nil
  ): this.type = ???
  def setMeta(meta: ir.Meta): this.type = ???
  def anonymize: this.type = ???
  def setName(name: String): this.type = ???
end DFC
object DFC:
  def emptyNoEO: DFC = ???
end DFC

into opaque type DFCG <: DFC = DFC
protected trait DFCGLP:
  inline given DFCG = DFCG()
object DFCG extends DFCGLP:
  def apply(): DFCG = DFC.emptyNoEO
  given DFCG(using dfc: DFC): DFCG = dfc
