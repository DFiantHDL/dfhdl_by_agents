package dfhdl.core
import dfhdl.compiler.ir
private[dfhdl] trait Domain extends Container:
  private[core] type TScope = DFC.Scope.Domain
  private[core] type TOwner = Domain.Block
  final protected given TScope = DFC.Scope.Domain
  final private[dfhdl] def initOwner: TOwner = ???
object Domain:
  type Block = DFOwner[ir.DomainBlock]
end Domain
