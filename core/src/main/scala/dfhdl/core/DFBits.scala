package dfhdl.core

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  given [W <: IntP & Singleton]: DFBits[W] = ???
end DFBits
