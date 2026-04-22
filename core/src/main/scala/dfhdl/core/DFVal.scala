package dfhdl.core

final class DFVal[+T <: DFType, +M]

type DFValAny = DFVal[DFType, ModifierAny]
type DFConstOf[+T <: DFType] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFType, +P] = DFVal[T, Modifier[P]]

inline def isConstCheck: Boolean = ${ ??? }

object DFVal:
  export DFXInt.Val.Ops.{evOpCarryAddSubDFXInt, evOpCarryMulDFXInt}
  object Ops:
    trait BoolOnlyOp
    trait CarryOp


