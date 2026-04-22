package dfhdl.core
import dfhdl.internals.*

protected[core] def analyzeControlRet(ret: Any)(using DFC): DFTypeAny = ???

extension [C <: DFValOf[DFBoolOrBit], T, F, TT <: DFTypeAny, TP, FT <: DFTypeAny, FP]
    (ifWrapper: IfWrapper[C, T, F])(using
    tcT: Exact0.TC[T, DFC] { type Out = DFValTP[TT, TP] },
    tcF: DFVal.TC[TT, F] { type OutP = FP }
)
  def unwrap(using dfc: DFC): DFValTP[TT, TP | FP] = ???
end extension

object DFIf
