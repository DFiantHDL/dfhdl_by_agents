package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  object DFVal:
    object Func:
      trait Op
      object Op:
        object `+` extends Op
        object `-` extends Op

trait ExactOp2Aux[Op, C, B, L, R, O]

sealed class Modifier[+P]
type ModifierAny = Modifier[Any]
object Modifier:
  type CONST = Modifier[Any]

final class DFType

final class DFC(mutableDB: MutableDB)

object DFBoolOrBit:
  object Val:
    object Ops:
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit2[Op <: FuncOp, L, R, O <: DFValAny](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] = ???

object DFDecimal:
  object Val:
    object Ops:
      export DFXInt.Val.Ops.*

object DFXInt:
  object Val:
    trait Candidate[P]

    object Ops:
      type CommutativeArithOp = FuncOp.+.type
      type NonCommutativeArithOp = FuncOp.-.type
      given evOpCommutativeArithDFXInt[Op <: CommutativeArithOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[Op <: NonCommutativeArithOp, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[Op, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

      given evOpCarryMulDFXInt[Op, LP, RP](using
          icL: Candidate[LP],
          icR: Candidate[RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

type DFConstInt32 = DFConstOf[DFType]
