package dfhdl.core
import dfhdl.compiler.ir
final class DFRange[P](val irValue: ir.DFRange | DFError) extends AnyVal with DFMember[ir.DFRange]
object DFRange:
  trait ScalaRangesFlag
  object Ops
