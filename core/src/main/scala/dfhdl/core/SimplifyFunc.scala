package dfhdl.core
import dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp

private object SimplifyFunc:
  def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] = None
