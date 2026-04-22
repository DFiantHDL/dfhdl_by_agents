package dfhdl.core
import dfhdl.internals.*
import dfhdl.hw
import dfhdl.compiler.ir.{
  DB,
  DuplicateTag,
  DFDesignInst,
  DFDesignInstOld,
  DFDesignBlock,
  DFMember,
  DFOwner,
  DFRef,
  DFRefAny,
  DFTag,
  DFVal,
  DFType,
  DomainBlock,
  MemberGetSet,
  SourceFile,
  MemberView,
  RTDomainCfg,
  DFTags,
  annotation,
  DFDomainOwner,
  DFInterfaceOwner,
  Meta
}
import dfhdl.compiler.analysis.filterPublicMembers

import scala.reflect.{ClassTag, classTag}
import collection.mutable
import collection.immutable.ListMap

private case class MemberEntry(
    irValue: DFMember,
    refSet: Set[DFRefAny],
    ignore: Boolean
)

class DesignContext:
  val members = mutable.ArrayBuffer.empty[MemberEntry]
  val memberTable = mutable.Map.empty[DFMember, Int]
  val refTable = mutable.Map.empty[DFRefAny, DFMember]
  val originRefTable = mutable.Map.empty[DFRef.TwoWayAny, DFMember]
  val unreachableNamedValues = mutable.Map.empty[DFVal, DFVal]
  val unreachableDFTypes = mutable.Map.empty[DFType, DFType]
  var defInputs = List.empty[DFValAny]
  val loopIterMap = mutable.Map.empty[Meta, DFValAny]
  var isDuplicate = false

  def setOriginRefs(member: DFMember): Unit = ???
  def addMember[M <: DFMember](member: M): M = ???
  def plantMember[M <: DFMember](
      owner: DFOwner | DFMember.Empty,
      member: M,
      updateOwnerCond: DFOwner => Boolean = _.isInstanceOf[DFDesignBlock]
  )(using MemberGetSet): M = ???
  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R = ???
  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M = ???
  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M = ???
  def ignoreMember[M <: DFMember](member: M): M = ???
  def hasMember(member: DFMember): Boolean = ???
  def getMemberRefs(member: DFMember): Set[DFRefAny] = ???
  def getLatestMember: DFMember = ???
  def inject(sourceCtx: DesignContext): Unit = ???
  def getImmutableMemberList: List[DFMember] = ???
  def getImmutableRefTable: Map[DFRefAny, DFMember] = ???
  def getReachableNamedValue(dfVal: DFVal, cf: => DFVal): DFVal = ???
  def getReachableDFType(dfType: DFType, cf: => DFType): DFType = ???
end DesignContext

final class MutableDB():
  private val self = this

  // error logger
  val logger = new Logger

  // meta programming external MemberGetSet DB access
  private[MutableDB] var metaGetSetList: List[MemberGetSet] = Nil
  def inMetaProgramming: Boolean = metaGetSetList.nonEmpty
  def injectMetaGetSet(metaGetSet: MemberGetSet): Unit =
    metaGetSetList = metaGetSet :: metaGetSetList

  object DesignContext:
    val global: DesignContext = new DesignContext
    var current: DesignContext = global
    var stack = List.empty[DesignContext]
    val designMembers = mutable.Map.empty[DFDesignBlock, List[DFMember]]
    val uniqueDesigns = mutable.Map.empty[String, List[List[DFDesignBlock]]]
    def startDesign(design: DFDesignBlock): Unit = ???
    def endDesign(design: DFDesignBlock): Unit = ???
    def runFuncWithInputs[V <: DFValAny](func: => V, inputs: List[DFValAny]): (Boolean, V) = ???
    def getDefInput(idx: Int): DFValAny = ???
    def addLoopIter(meta: Meta, iter: DFValAny): Unit = ???
    def getLoopIter(meta: Meta): DFValAny = ???
    def getMembersNum: Int = ???
    def getMembers(from: Int, until: Int): List[DFMember] = ???
    def getLastMembers(cnt: Int): List[DFMember] = ???
    def getLastDesignInst: DFDesignBlock = ???
    def getReachableNamedValue(dfVal: DFVal, cf: => DFVal): DFVal = ???
    def getReachableDFType(dfType: DFType, cf: => DFType): DFType = ???
  end DesignContext

  val injectedCtx = mutable.Set.empty[DesignContext]
  def injectGlobals(sourceCtx: DesignContext): Unit =
    // preventing meta-programming global injection to avoid duplicates
    if (!inMetaProgramming && !injectedCtx.contains(sourceCtx))
      injectedCtx += sourceCtx
      DesignContext.global.inject(sourceCtx)

  object OwnershipContext:
    def enter(owner: DFOwner): Unit = ???
    def exit(): Unit = ???
    def exitLastDesign(): Unit = ???
    def enterLate(): Unit = ???
    def exitLate(): Unit = ???
    def owner: DFOwner = ???
    def currentDesign: DFDesignBlock = ???
    def lateConstruction: Boolean = ???
    def replaceOwner(originalOwner: DFOwner, newOwner: DFOwner): Unit = ???
    def containerizedOwnerOfRef(ref: DFRefAny): DFDomainOwner = ???
    def ownerOption: Option[DFOwner] = ???
  end OwnershipContext

  object ResourceOwnershipContext:
    import dfhdl.platforms.resources.*
    def getConnectedDclResourceMap: Map[DFVal.Dcl, List[(Range, Resource)]] = ???
    def connectDclResource(dcl: DFVal.Dcl, range: Range, resource: Resource): Unit = ???
    def connectDomainOwner(domainOwner: DFDomainOwner, clkResource: ClkResource): Unit = ???
    def replaceDcl(fromPort: DFVal.Dcl, toPort: DFVal.Dcl): Unit = ???
    def getConstrainedDcls(): Map[DFVal.Dcl, DFVal.Dcl] = ???
    def getConstrainedDomainOwner(domainOwner: DFDomainOwner): DFDomainOwner = ???
    def getTopResourceOwners: List[ResourceOwner] = ???
    def emptyTopResourceOwners(): Unit = ???
    def enter(owner: ResourceOwner): Unit = ???
    def exit(): Unit = ???
    def owner: ResourceOwner = ???
    def ownerOpt: Option[ResourceOwner] = ???
  end ResourceOwnershipContext

  object GlobalTagContext:
    private[MutableDB] var tags: DFTags = DFTags.empty
    def set[CT <: DFTag: ClassTag](tag: CT): Unit = tags = tags.tag(tag)
    def get[CT <: DFTag: ClassTag]: Option[CT] = tags.getTagOf[CT]
  end GlobalTagContext

  def addMember[M <: DFMember](member: M): M = ???
  def plantMember[M <: DFMember](
      owner: DFOwner | DFMember.Empty,
      member: M,
      updateOwnerCond: DFOwner => Boolean = _.isInstanceOf[DFDesignBlock]
  ): M = ???
  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R = ???
  def getMemberOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] = ???
  def getMember[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 = ???
  def getOriginMember(ref: DFRef.TwoWayAny): DFMember = ???
  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M = ???
  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M = ???
  def ignoreMember[M <: DFMember](member: M): M = ???
  def immutable: DB = ???
  given getSet: MemberGetSet = ???
end MutableDB
