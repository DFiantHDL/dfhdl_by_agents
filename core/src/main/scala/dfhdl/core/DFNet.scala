package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

final class DFNet(val irValue: ir.DFNet | DFError) extends AnyVal with DFMember[ir.DFNet]
object DFNet:
  export ir.DFNet.Op
  extension (net: ir.DFNet) def asFE: DFNet = ???
  def apply(toVal: ir.DFVal, op: Op, fromVal: ir.DFVal)(using DFC): DFNet = ???
end DFNet
