package dfhdl.core
import dfhdl.compiler.ir

extension [M <: ir.DFMember](member: M)
  private[dfhdl] def injectGlobalCtx()(using DFC): Unit = ???
  private[core] def getReachableMember(using dfc: DFC): M = ???
  def ref(using DFC): ir.DFRef.OneWay[M] = ???
  def refTW[O <: ir.DFMember](using dfc: DFC): ir.DFRef.TwoWay[M, O] = ???
  def refTW[O <: ir.DFMember](knownReachable: Boolean)(using dfc: DFC): ir.DFRef.TwoWay[M, O] = ???
end extension

extension [T <: ir.DFOwner](owner: DFOwner[T])
  def ref(using DFC): ir.DFRef.OneWay[T] = ???
