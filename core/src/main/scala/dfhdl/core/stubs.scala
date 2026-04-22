package dfhdl.core
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import NativeType.*

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  trait DFType
  trait DFBoolOrBit extends DFType
  trait DFBool extends DFType
  object DFBool extends DFBool
  trait DFDecimal extends DFType
  trait DFVal
  val DFInt32: DFType = ???
  object DFVal:
    object Func:
      sealed trait Op
      object Op:
        sealed trait `+` extends Op; object `+` extends `+`
        sealed trait `-` extends Op; object `-` extends `-`
  object DFDecimal:
    sealed trait NativeType
    object NativeType:
      sealed trait Int32 extends NativeType
end ir

trait ExactOp2Aux[Op, C, B, L, R, O]

sealed class Modifier[+A, +C, +I, +P]
type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  type CONST = Modifier[Any, Any, Any, Any]

export DFType.asFE

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args4[T1, T2, T3, T4] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T) extends AnyVal
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
  export DFBoolOrBit.given
  export DFDecimal.given
end DFType

final case class DFC(
    mutableDB: MutableDB = new MutableDB()
)

type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  given DFBool = DFBool

  object Val:
    object Ops:
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit2[Op <: FuncOp, L, R, O <: DFValAny](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] = ???
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]

type DFDecimal[S, W, F, N] = DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  object Val:
    object Ops:
      export DFXInt.Val.Ops.*
end DFDecimal

object DFXInt:
  object Val:
    trait Candidate[R]:
      type OutP
    object Candidate:
      type Aux[R, P] = Candidate[R] { type OutP = P }

    object Ops:
      type CommutativeArithOp = FuncOp.+.type
      type NonCommutativeArithOp = FuncOp.-.type
      given evOpCommutativeArithDFXInt[
          Op <: CommutativeArithOp, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFTypeAny, LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[
          Op <: NonCommutativeArithOp, L, LP, R, RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFTypeAny, LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[Op <: FuncOp, L, LP, R, RP](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[DFTypeAny, LP | RP]] = ???

      given evOpCarryMulDFXInt[Op <: FuncOp, L, LP, R, RP](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[DFTypeAny, LP | RP]] = ???
    end Ops
  end Val
end DFXInt

type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]]
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
