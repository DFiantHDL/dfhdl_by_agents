package dfhdl.core
import ir.DFVal.Func.Op as FuncOp
import scala.quoted.*

into final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal)

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[P]]

inline def isConstCheck[T]: Boolean = ${ ??? }

object DFVal:
  export DFXInt.Val.Ops.{
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }
  object Ops:
    protected[core] trait BoolOnlyOp[Op <: FuncOp]
    protected[core] trait CarryOp[Op <: FuncOp]
end DFVal


