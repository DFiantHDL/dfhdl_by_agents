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
      given bl[Op, L, R, O](using
          ExactOp2Aux[Op, DFC, Any, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp, DFC, Any, L, R, O] = ???

object DFDecimal:
  object Val:
    object Ops:
      export DFXInt.Val.Ops.*

object DFXInt:
  object Val:
    object Ops:
      type A = Int
      type B = String
      given arith1[Op <: A]: ExactOp2Aux[Op, DFC, Any, Any, Any, DFValTP[DFType, Any]] = ???
      given arith2[Op <: B]: ExactOp2Aux[Op, DFC, Any, Any, Any, DFValTP[DFType, Any]] = ???

      import DFVal.Ops.CarryOp
      given c1: ExactOp2Aux[CarryOp, DFC, Any, Any, Any, DFValTP[DFType, Any]] = ???
      given c2: ExactOp2Aux[CarryOp, DFC, Any, Any, Any, DFValTP[DFType, Any]] = ???

type DFConstInt32 = DFConstOf[DFType]
