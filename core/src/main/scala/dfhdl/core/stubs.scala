package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  trait DFType
  trait DFBoolOrBit extends DFType
  trait DFVal
  object DFVal:
    object Func:
      sealed trait Op
      object Op:
        object `+` extends Op
        object `-` extends Op
end ir

trait ExactOp2Aux[Op, C, B, L, R, O]

sealed class Modifier[+P]
type ModifierAny = Modifier[Any]
object Modifier:
  type CONST = Modifier[Any]

export DFType.asFE

sealed trait Args
sealed trait NoArgs extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T) extends AnyVal
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = ???
end DFType

final case class DFC(mutableDB: MutableDB)

type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  object Val:
    object Ops:
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit2[Op <: FuncOp, L, R, O <: DFValAny](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] = ???
  end Val
end DFBoolOrBit

object DFDecimal:
  object Val:
    object Ops:
      export DFXInt.Val.Ops.*
end DFDecimal

object DFXInt:
  object Val:
    trait Candidate[P]

    object Ops:
      type CommutativeArithOp = FuncOp.+.type
      type NonCommutativeArithOp = FuncOp.-.type
      given evOpCommutativeArithDFXInt[Op <: CommutativeArithOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFTypeAny, LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[Op <: NonCommutativeArithOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFTypeAny, LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[Op <: FuncOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, Any, Any, DFValTP[DFTypeAny, LP | RP]] = ???

      given evOpCarryMulDFXInt[Op <: FuncOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, Any, Any, DFValTP[DFTypeAny, LP | RP]] = ???
    end Ops
  end Val
end DFXInt

type DFConstInt32 = DFConstOf[DFTypeAny]
