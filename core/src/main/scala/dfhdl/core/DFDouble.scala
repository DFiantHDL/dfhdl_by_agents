package dfhdl.core
import dfhdl.compiler.ir
type DFDouble = TDFDouble
final val DFDouble = ir.DFDouble.asFE[DFDouble]

type TDFDouble = DFType[ir.DFDouble, NoArgs]
object TDFDouble:
  given DFDouble = DFDouble
  object Val:
    object TC
    object Compare
    object Ops
