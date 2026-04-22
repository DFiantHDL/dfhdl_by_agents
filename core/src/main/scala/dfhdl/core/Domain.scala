package dfhdl.core
import dfhdl.compiler.ir
import scala.reflect.ClassTag
private[dfhdl] trait Domain extends Container with scala.reflect.Selectable:
  private[core] type TScope = DFC.Scope.Domain
  private[core] type TOwner = Domain.Block
  final protected given TScope = DFC.Scope.Domain
  final private[dfhdl] def initOwner: TOwner = ???
  final override def onCreateEnd(thisOwner: Option[This]): Unit = ???

object Domain:
  type Block = DFOwner[ir.DomainBlock]
  object Block:
    def apply(domainType: ir.DomainType)(using DFC): Block = ???
  end Block
  extension [D <: Domain](domain: D)
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): D = ???
    infix def setName(name: String)(using dfc: DFC): D = ???
  end extension

end Domain

trait NoClkRstDomain extends Domain:
  protected inline def Clk: DFOpaque[DFOpaque.Clk] =
    compiletime.error("Clk/Rst declarations are not allowed in this domain.")
  protected inline def Rst: DFOpaque[DFOpaque.Rst] =
    compiletime.error("Clk/Rst declarations are not allowed in this domain.")

abstract class DFDomain extends DomainContainer(DomainType.DF), NoClkRstDomain

abstract class RTDomain(
    cfg: RTDomainCfg = RTDomainCfg.Derived
) extends RTDomainContainer(cfg),
      Domain:
  related =>
  abstract class RelatedDomain extends RTDomain(RTDomainCfg.Related(related))

abstract class EDDomain extends DomainContainer(DomainType.ED), NoClkRstDomain
