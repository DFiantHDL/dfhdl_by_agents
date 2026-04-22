package dfhdl.core
import dfhdl.compiler.ir

trait Design extends Container:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[dfhdl] def initOwner: TOwner = ???
end Design

object Design:
  type Block = DFOwner[ir.DFDesignBlock]
end Design
