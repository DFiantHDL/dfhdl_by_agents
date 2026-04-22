package dfhdl.core
import dfhdl.compiler.ir
type DFString = TDFString
final val DFString = new DFType(ir.DFString).asInstanceOf[DFString]

type TDFString = DFType[ir.DFString, NoArgs]
object TDFString:
  given DFString = DFString
  object Val:
    object TC
    object Compare
    object Ops
