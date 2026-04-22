package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName

type DFPhysical[U <: ir.PhysicalNumber] = DFType[ir.DFPhysical[U], NoArgs]
object DFPhysical:
  object Val:
    object Ops
end DFPhysical

type DFTime = DFPhysical[ir.TimeNumber]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[ir.FreqNumber]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[ir.LiteralNumber]
val DFNumber = ir.DFNumber.asFE[DFNumber]
