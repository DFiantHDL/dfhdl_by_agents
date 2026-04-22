package dfhdl.core
import scala.annotation.Annotation
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import NativeType.*
import scala.quoted.*

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  trait DFType
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
end ir

trait Position
trait MetaContext
trait ExactOp2Aux[Op, C, B, L, R, O]

sealed class Modifier[+A, +C, +I, +P]
type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  type CONST = Modifier[Any, Any, Any, Any]

type IntP = Any

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
  export DFDecimal.given
end DFType

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

type BitNum = 0 | 1
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  given DFBool = DFBool

  object Val:
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      type OutP
    object Candidate:
      type Types = DFValOf[DFBoolOrBit] | Boolean | BitNum
      type Aux[R, T <: DFBoolOrBit, P] = Candidate[R] { type OutT = T; type OutP = P }
    end Candidate

    object Ops:
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          L <: Candidate.Types,
          LT <: DFBoolOrBit,
          LP,
          R <: Candidate.Types,
          RT <: DFBoolOrBit,
          RP
      ](using
          icL: Candidate.Aux[L, LT, LP],
          icR: Candidate.Aux[R, RT, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[LT, LP | RP]] = ???
      given evLogicOpDFBoolOrBit2[
          Op <: FuncOp.|.type | FuncOp.&.type,
          L <: Candidate.Types,
          R <: Candidate.Types,
          O <: DFValAny
      ](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] = ???

    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]

type DFDecimal[S <: Boolean, W <: IntP, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  object StrInterpOps:
    extension (inline sc: StringContext)
      transparent inline def apply(inline args: Any*)(using dfc: DFCG): Any =
        ${ applyMacro('sc, 'args)('dfc) }
    private def applyMacro(
        sc: Expr[StringContext],
        args: Expr[Seq[Any]]
    )(dfc: Expr[DFC])(using Quotes): Expr[Any] = ???
  end StrInterpOps

  object Val:
    object Ops:
      export DFXInt.Val.Ops.*
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: IntP, N <: NativeType] = DFDecimal[S, W, 0, N]
object DFXInt:
  object Val:
    trait Candidate[R]:
      type OutP
    object Candidate:
      type Aux[R, P] = Candidate[R] { type OutP = P }

    object Ops:
      type CommutativeArithOp =
        FuncOp.+.type | FuncOp.*.type | FuncOp.max.type | FuncOp.min.type
      type NonCommutativeArithOp =
        FuncOp.-.type | FuncOp./.type | FuncOp.%.type
      given evOpCommutativeArithDFXInt[
          Op <: CommutativeArithOp, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[
          Op <: NonCommutativeArithOp, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[
          Op <: FuncOp.+.type | FuncOp.-.type, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???

      given evOpCarryMulDFXInt[
          Op <: FuncOp.`*`.type, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???
    end Ops
  end Val
end DFXInt

type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]]
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
