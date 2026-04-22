package dfhdl.core

trait ExactOp2Aux[Op, C, O]

trait Modifier[+P]
type ModifierAny = Modifier[Any]
object Modifier:
  type CONST = Modifier[Any]

final class DFType

final class DFC(mutableDB: MutableDB)

object DFBoolOrBit:
  object Ops:
    import DFVal.Ops.BoolOnlyOp
    given bl[Op, O](using
        ExactOp2Aux[Op, DFC, O]
    ): ExactOp2Aux[BoolOnlyOp, DFC, O] = ???

object DFDecimal:
  object Ops:
    export DFXInt.Ops.*

object DFXInt:
  object Ops:
    type A = Int
    type B = String
    given arith1[Op <: A]: ExactOp2Aux[Op, DFC, DFValTP[DFType, Any]] = ???
    given arith2[Op <: B]: ExactOp2Aux[Op, DFC, DFValTP[DFType, Any]] = ???

    import DFVal.Ops.CarryOp
    given c1: ExactOp2Aux[CarryOp, DFC, DFValTP[DFType, Any]] = ???
    given c2: ExactOp2Aux[CarryOp, DFC, DFValTP[DFType, Any]] = ???

type DFConstInt32 = DFConstOf[DFType]
