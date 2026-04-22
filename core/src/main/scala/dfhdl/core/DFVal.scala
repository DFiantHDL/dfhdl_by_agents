package dfhdl.core

final class DFVal[+T <: DFType, +M]

type DFValAny = DFVal[DFType, ModifierAny]
type DFConstOf[+T <: DFType] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFType, +P] = DFVal[T, Modifier[P]]

inline def x: Any = ${ ??? }

object DFVal:
  export DFXInt.Ops.{c1, c2}
  object Ops:
    trait BoolOnlyOp
    trait CarryOp


