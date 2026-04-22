package dfhdl.core

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
      given evLogicOpDFBoolOrBit2[Op, L, R, O <: DFValAny](using
          ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp, DFC, DFValAny, L, R, O] = ???

object DFDecimal:
  object Val:
    object Ops:
      export DFXInt.Val.Ops.*

object DFXInt:
  object Val:
    trait Candidate[P]

    object Ops:
      type CommutativeArithOp = Int
      type NonCommutativeArithOp = String
      given evOpCommutativeArithDFXInt[Op <: CommutativeArithOp, LP, RP](using
          Candidate[LP], Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[Op <: NonCommutativeArithOp, LP, RP](using
          Candidate[LP], Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[LP, RP](using
          Candidate[LP], Candidate[RP]
      ): ExactOp2Aux[CarryOp, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

      given evOpCarryMulDFXInt[LP, RP](using
          Candidate[LP], Candidate[RP]
      ): ExactOp2Aux[CarryOp, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???

type DFConstInt32 = DFConstOf[DFType]
