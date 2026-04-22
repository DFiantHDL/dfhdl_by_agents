package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

final class DFVal[+T <: DFType, +M]

type DFValAny = DFVal[DFType, ModifierAny]
type DFConstOf[+T <: DFType] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFType, +P] = DFVal[T, Modifier[P]]

inline def isConstCheck: Boolean = ${ ??? }

object DFVal:
  export DFXInt.Val.Ops.{evOpCarryAddSubDFXInt, evOpCarryMulDFXInt}
  object Ops:
    trait BoolOnlyOp[Op]
    trait CarryOp[Op]


