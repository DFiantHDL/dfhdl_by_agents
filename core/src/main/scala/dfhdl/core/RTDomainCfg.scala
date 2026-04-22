package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

type ClkCfg = ir.ClkCfg
object ClkCfg:
  type Rate = DFConstOf[DFTime | DFFreq]
  def apply(): ClkCfg = ???
end ClkCfg

type RstCfg = ir.RstCfg
object RstCfg:
  def apply(): RstCfg = ???

opaque type RTDomainCfg <: ir.RTDomainCfg = ir.RTDomainCfg
object RTDomainCfg:
  def forced(name: String, clkCfg: ClkCfg, rstCfg: RstCfg): RTDomainCfg = ???
  def apply(clkCfg: ClkCfg, rstCfg: RstCfg)(using ctName: CTName): RTDomainCfg = ???
  val Comb: RTDomainCfg = ???
  def Default(using dfc: DFC): RTDomainCfg = ???
  val Derived: RTDomainCfg = ???
  extension (cfg: RTDomainCfg)
    def asIR: ir.RTDomainCfg = cfg
    def norst: RTDomainCfg = ???
  extension (cfg: ir.RTDomainCfg) def asFE: RTDomainCfg = cfg
  protected[core] object Related:
    def apply(design: RTDesign)(using DFC): RTDomainCfg = ???
    def apply(domain: RTDomain)(using DFC): RTDomainCfg = ???
end RTDomainCfg
