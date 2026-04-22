package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

final class DFVal[+T <: DFType, +M <: ModifierAny]

type DFValAny = DFVal[DFType, ModifierAny]
type DFConstOf[+T <: DFType] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFType, +P] = DFVal[T, Modifier[P]]

inline def isConstCheck: Boolean = ${ ??? }

object DFVal:
  export DFXInt.Val.Ops.{
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }
  object Ops:
    protected[core] trait BoolOnlyOp[Op <: FuncOp]
    protected[core] trait CarryOp[Op <: FuncOp]
end DFVal


