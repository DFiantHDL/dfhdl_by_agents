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

  def addMember[M <: DFMember](member: M): M =
    dirtyDB()
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        dfVal.globalCtx = DesignContext.global
      case design: DFDesignBlock =>
        DesignContext.startDesign(design)
      case _ =>
    DesignContext.current.addMember(member)

  // same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
  def plantMember[M <: DFMember](
      owner: DFOwner | DFMember.Empty,
      member: M,
      updateOwnerCond: DFOwner => Boolean = _.isInstanceOf[DFDesignBlock]
  ): M =
    dirtyDB()
    DesignContext.current.plantMember(owner, member, updateOwnerCond) // (using metaGetSetList.last)

  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R =
    dirtyDB()
    DesignContext.current.newRefFor(ref, member)

  def getMemberOption[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): Option[M0] =
    // by default the current design context is searched
    val memberOption: Option[DFMember] = DesignContext.current.refTable.get(ref) match
      case some: Some[DFMember] => some
      // if we didn't find it, then we go up the design context stack
      case None =>
        DesignContext.stack.view
          .map(_.refTable.get(ref))
          .collectFirst { case Some(member) => member }
          // finally, if still no member is available, then we check the
          // external injected meta-programming context
          .orElse(metaGetSetList.view.flatMap(_.getOption(ref)).headOption)
    memberOption.asInstanceOf[Option[M0]]
  end getMemberOption

  def getMember[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): M0 = getMemberOption(ref).getOrElse(
    throw new IllegalArgumentException(s"Missing ref $ref")
  )

  def getOriginMember(
      ref: DFRef.TwoWayAny
  ): DFMember =
    // by default the current design context is searched
    val member = DesignContext.current.originRefTable.get(ref) match
      case Some(member) => member
      // if we didn't find it, then we go up the design context stack
      case None =>
        DesignContext.stack.view
          .map(_.originRefTable.get(ref))
          .collectFirst { case Some(member) => member }
          // finally, if still no member is available, then we check the
          // external injected meta-programming context
          .getOrElse(
            metaGetSetList.view.flatMap(_.getOption(ref)).headOption.getOrElse(
              throw new IllegalArgumentException(s"Missing ref $ref")
            )
          )
    member
  end getOriginMember

  private def globalMemberCtxUpdate(member: DFMember): Unit =
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        dfVal.globalCtx = DesignContext.global
      case _ =>

  // if the original member is global, then injects its context into
  // the current context
  private def globalMemberCtxInject(member: DFMember): Unit =
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        injectGlobals(dfVal.globalCtx.asInstanceOf[DesignContext])
      case _ =>

  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
    if (inMetaProgramming) newMemberFunc(originalMember)
    else
      dirtyDB()
      globalMemberCtxInject(originalMember)
      val newMember = DesignContext.current.setMember(originalMember, newMemberFunc)
      globalMemberCtxUpdate(newMember)
      // in case the member is an owner, we check the owner stack to replace it.
      // and for ports, we update the connected resource map.
      (originalMember, newMember) match
        case (o: DFOwner, n: DFOwner) =>
          OwnershipContext.replaceOwner(o, n)
        case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
          ResourceOwnershipContext.replaceDcl(fromPort, toPort)
        case _ =>
      newMember
  end setMember

  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
    dirtyDB()
    globalMemberCtxInject(originalMember)
    DesignContext.current.replaceMember(originalMember, newMember)
    globalMemberCtxUpdate(newMember)
    // in case the member is an owner, we check the owner stack to replace it.
    // and for ports, we update the connected resource map.
    (originalMember, newMember) match
      case (o: DFOwner, n: DFOwner) =>
        OwnershipContext.replaceOwner(o, n)
      case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
        ResourceOwnershipContext.replaceDcl(fromPort, toPort)
      case _ =>
    newMember
  end replaceMember

  def ignoreMember[M <: DFMember](
      member: M
  ): M = // ignoring it means removing it for the immutable DB
    dirtyDB()
    DesignContext.current.ignoreMember(member)

  private def dirtyDB(): Unit = memoizedDB = None
  private var memoizedDB: Option[DB] = None

  def getFlattenedMemberList(topMemberList: List[DFMember]): List[DFMember] =
    def flattenMembers(owner: DFMember): List[DFMember] = owner match
      case o: DFDesignBlock =>
        o :: DesignContext.designMembers.getOrElse(o, Nil).flatMap(flattenMembers)
      case member => List(member)
    topMemberList.flatMap(flattenMembers)

  def immutable: DB = memoizedDB.getOrElse {
    // if in meta-programming (indicated by the existence of an external context),
    // then we need to just get the current hierarchy members and refTable
    val (members, refTable) =
      if (inMetaProgramming)
        (DesignContext.current.getImmutableMemberList, DesignContext.current.getImmutableRefTable)
      // otherwise we first flatten the hierarchy and then make sure all design
      // declarations are unique and tag duplicate instances accordingly.
      else
        val members =
          getFlattenedMemberList(DesignContext.current.getImmutableMemberList)
        val refTable = DesignContext.current.getImmutableRefTable
        // removing unused type references due to `dropUnreachableRefs`
        val usedTypeRefs = members.view.flatMap {
          case dfVal: DFVal => dfVal.getRefs.collect { case r: DFRef.TypeRef => r }
          case _            => Nil
        }.toSet
        val fixedRefTable = refTable.view.filter {
          case (ref: DFRef.TypeRef, m) => usedTypeRefs.contains(ref)
          case _                       => true
        }.toMap
        val duplicateDesignSet = mutable.Set.empty[DFDesignBlock]
        val duplicateDesignRepMap = DesignContext.uniqueDesigns.view.flatMap {
          case (designType, groupList) =>
            groupList.view.reverse.zipWithIndex.flatMap {
              case (group, i) if group.length > 1 || groupList.length > 1 =>
                val updatedDclName =
                  if (groupList.length > 1) s"${designType}_${i.toPaddedString(groupList.length)}"
                  else designType
                var first = true
                group.view.map(design =>
                  val tags =
                    if (first)
                      first = false
                      design.tags
                    else
                      duplicateDesignSet += design
                      design.tags.tag(DuplicateTag)
                  design -> design.copy(
                    meta = design.meta.copy(nameOpt = Some(updatedDclName)),
                    tags = tags
                  )
                )
              case _ => Nil
            }
        }.toMap
        // replacement map for domain owners that includes both duplicated designs and constrained domain owners
        val domainOwnerRepMap = members.collect {
          case design: DFDesignBlock =>
            design -> ResourceOwnershipContext.getConstrainedDomainOwner(
              duplicateDesignRepMap.getOrElse(design, design)
            )
          case domainOwner: DFDomainOwner =>
            domainOwner -> ResourceOwnershipContext.getConstrainedDomainOwner(domainOwner)
        }.toMap
        // apply connected resource constraints to their connected ports
        val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
        // apply the final fixes to the members:
        // 1. replace duplicate design instances and constrained domain owners
        // 2. apply connected resource constraints to their connected ports
        val finalFixFunc: DFMember => DFMember = {
          case domainOwner: DFDomainOwner => domainOwnerRepMap(domainOwner)
          case dcl: DFVal.Dcl             => constrainedDcls.getOrElse(dcl, dcl)
          case m                          => m
        }
        // Remove all remaining public members (ports, domain blocks, and their
        // dependencies) from duplicate designs. During elaboration these were kept
        // so user code could reference them, but in the immutable DB they are no
        // longer needed. Ports for duplicate designs are resolved on-demand via
        // DuplicationRef in `DB.dupPortsByName`.
        val redundantRefs = mutable.Set.empty[DFRefAny]
        val finalMembers = members.flatMap {
          case m: DFVal if m.isGlobal => Some(finalFixFunc(m))
          case m: (DomainBlock | DFVal) if duplicateDesignSet.contains(m.getOwnerDesign) =>
            redundantRefs += m.ownerRef
            redundantRefs ++= m.getRefs
            None
          case m => Some(finalFixFunc(m))
        }
        val finalRefTable = fixedRefTable.view.flatMap { case (ref, member) =>
          if (redundantRefs.contains(ref)) None else Some(ref -> finalFixFunc(member))
        }.toMap
        (finalMembers, finalRefTable)
    val membersNoGlobalCtx = members.map {
      case m: DFVal.CanBeGlobal  => m.copyWithoutGlobalCtx
      case design: DFDesignBlock =>
        design.clearDesignInstCache()
        design
      case m => m
    }
    val globalTags = GlobalTagContext.tags
    // Drop orphan OneWay.Gen refs — refTable entries whose key is no live
    // member's ownerRef. Elaboration scaffolding (especially meta-design /
    // cloneAnon paths) can leak these; they're safe to drop because no
    // member emits them.
    val cleanedRefTable =
      val memberOwnerRefs = mutable.Set.empty[DFRefAny]
      membersNoGlobalCtx.foreach { m =>
        memberOwnerRefs += m.ownerRef
        m match
          // DFDesignInst's designRef is a OneWay.Gen ref outside of getRefs
          // (which only carries TwoWay refs); keep it from being swept away.
          case inst: DFDesignInst => memberOwnerRefs += inst.designRef
          case _                  =>
      }
      refTable.filter { (r, _) =>
        r match
          case _: DFRef.OneWay.Gen[?] => memberOwnerRefs.contains(r)
          case _                      => true
      }
    end cleanedRefTable
    val db = DB(membersNoGlobalCtx, cleanedRefTable, globalTags, Nil)
    memoizedDB = Some(db)
    db
  }

  given getSet: MemberGetSet with
    val isMutable: Boolean = true
    def designDB: DB = immutable
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 = getMember(ref)
    def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] = getMemberOption(ref)
    def getOrigin(ref: DFRef.TwoWayAny): DFMember = getOriginMember(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      setMember(originalMember, newMemberFunc)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M =
      replaceMember(originalMember, newMember)
    def remove[M <: DFMember](member: M): M = ignoreMember(member)
    def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit = GlobalTagContext.set(tag)
    def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT] = GlobalTagContext.get[CT]
    def findDesignInst(design: DFDesignBlock): Option[DFDesignInst] =
      metaGetSetList.view.flatMap(_.findDesignInst(design)).headOption
        // fall back to the immutable DB's reverse-lookup map, which covers
        // the normal elaboration path where metaGetSetList is empty
        // TODO: this is required for DFBitsSpec tests that uses a design def in assertCodeString.
        // this needs to be removed, but we'll leave it here until we get rid of DFDesignBlock duplicates completely.
        .orElse(designDB.designInstMap.get(design))
  end getSet

end MutableDB
