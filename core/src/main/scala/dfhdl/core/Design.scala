package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.Annotation
import ir.DFDesignBlock.InstMode

trait Design extends Container, HasClsMetaArgs:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode: InstMode = InstMode.Normal
  private[dfhdl] def initOwner: TOwner = ???
  final protected def setClsNamePos(name: String, position: Position, docOpt: Option[String], annotations: List[Annotation]): Unit = ???
end Design

object Design:
  type Block = DFOwner[ir.DFDesignBlock]
  object Block

abstract class DFDesign extends DomainContainer(DomainType.DF), Design

abstract class RTDesign(cfg: RTDomainCfg = RTDomainCfg.Derived) extends RTDomainContainer(cfg), Design:
  related =>
  abstract class RelatedDomain extends RTDomain(RTDomainCfg.Related(related))

abstract class EDDesign extends DomainContainer(DomainType.ED), Design

abstract class EDBlackBox(source: EDBlackBox.Source) extends EDDesign:
  override private[core] def mkInstMode: InstMode = InstMode.BlackBox(source)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
