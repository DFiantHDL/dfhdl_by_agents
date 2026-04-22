package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound
import ir.annotation.HWAnnotation

@implicitNotFound(
  "Missing local design context."
)
final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation] = Nil,
    mutableDB: MutableDB = new MutableDB(),
    refGen: ir.RefGen = ir.RefGen.initial,
    tags: ir.DFTags = ir.DFTags.empty
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt,
      annotations: List[Annotation] = Nil
  ): this.type = ???
  def setMeta(meta: ir.Meta): this.type = ???
  def setTags(tags: ir.DFTags): DFC = ???
  def tag[CT <: ir.DFTag: ClassTag](customTag: CT): DFC = ???
  def emptyTags: DFC = ???
  given getSet: ir.MemberGetSet = mutableDB.getSet
  def getMeta: ir.Meta = ???
  def enterOwner(owner: DFOwnerAny): Unit = ???
  def exitOwner(): Unit = ???
  def owner: DFOwnerAny = ???
  def enterLate(): Unit = ???
  def exitLate(): Unit = ???
  def lateConstruction: Boolean = ???
  def ownerOption: Option[DFOwnerAny] = ???
  def ownerOrEmptyRef: ir.DFOwner.Ref = ???
  def setName(name: String): this.type = ???
  def setAnnotations(annotations: List[HWAnnotation]): this.type = ???
  def anonymize: this.type = ???
  def logEvent(event: LogEvent): Unit = ???
  def injectEvents(newEvents: List[LogEvent]): Unit = ???
  def getErrors: List[DFError] = ???
  def getWarnings: List[DFWarning] = ???
  def getEvents: List[LogEvent] = ???
  def inMetaProgramming: Boolean = ???
  def clearEvents(): Unit = ???
end DFC
object DFC:
  def emptyNoEO: DFC = ???
  sealed trait Scope
  object Scope:
    sealed trait Global extends Scope
    object Global extends Global
    given Global = Global
    sealed trait Local extends Scope
    sealed trait Design extends Local
    object Design extends Design
    sealed trait Domain extends Local
    object Domain extends Domain
    sealed trait Process extends Local:
      // will include the step cache according to the name of the step block
      // (the plugin will make sure that the name is unique)
      private[core] val stepCache = mutable.Map.empty[String, ir.StepBlock]
    object Process extends Process
    sealed trait Interface extends Local
    object Interface extends Interface
  end Scope
end DFC

into opaque type DFCG <: DFC = DFC
protected trait DFCGLP:
  // DFCG given must be inline to force new DFC is generated for every missing DFC summon.
  inline given DFCG = DFCG()
object DFCG extends DFCGLP:
  def apply(): DFCG = DFC.emptyNoEO
  @metaContextIgnore
  given DFCG(using dfc: DFC): DFCG = dfc
  given Conversion[DFC, DFCG] = identity

transparent inline def dfc(using d: DFC): d.type = d

trait HasDFC:
  lazy val dfc: DFC
  protected given DFC = dfc
