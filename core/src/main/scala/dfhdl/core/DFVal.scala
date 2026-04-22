package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

final class DFVal[+T <: DFTypeAny, +M <: ModifierAny]

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[P]]

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


