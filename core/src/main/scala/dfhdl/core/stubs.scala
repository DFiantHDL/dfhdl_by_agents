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
      given evLogicOpDFBoolOrBit2[Op, L, R, O](using
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
      type A = Int
      type B = String
      given arith1[Op <: A, LP, RP](using
          Candidate[LP], Candidate[RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, Any, Any, DFValTP[DFType, LP | RP]] = ???
      given arith2[Op <: B, LP, RP](using
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
