package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.options.ElaborationOptions
import dfhdl.hw.annotation.getActiveHWAnnotations
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound
import ir.annotation.HWAnnotation

@implicitNotFound(
  "Missing local design context.\nEither this operation is not supported in global context or `using DFC` is missing."
)
final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation] = Nil, // TODO: removing default causes stale symbol crash
    mutableDB: MutableDB = new MutableDB(),
    refGen: ir.RefGen = ir.RefGen.initial,
    tags: ir.DFTags = ir.DFTags.empty,
    elaborationOptionsContr: () => ElaborationOptions = () =>
      summon[ElaborationOptions.Defaults[Design]]
) extends MetaContext:
  lazy val elaborationOptions: ElaborationOptions = elaborationOptionsContr()
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
  import java.util.concurrent.atomic.AtomicInteger

  /** Thread-safe cache for generating unique group IDs based on position hash codes.
    *
    * Thread Safety Guarantees:
    *   - Uses `TrieMap` for thread-safe concurrent access to the cache
    *   - Each hash code gets its own `AtomicInteger` counter for unique ID generation
    *   - `getOrElseUpdate` atomically checks and creates new counters if needed
    *   - `AtomicInteger.getAndIncrement()` provides atomic increment operations
    *
    * This design ensures that:
    *   1. Multiple threads can safely access the cache concurrently
    *   2. Each position hash code gets a unique incremental ID
    *   3. No race conditions occur during counter creation or increment
    *   4. Memory usage is bounded by the number of unique position hash codes
    */
  private val positionCache = collection.concurrent.TrieMap.empty[Int, AtomicInteger]

  /** Generates a unique group ID tuple for a given position.
    *
    * The tuple consists of:
    *   - First element: The position's hash code (for grouping similar positions)
    *   - Second element: A unique incremental ID for positions with the same hash code
    *
    * Thread Safety:
    *   - This method is thread-safe and can be called concurrently by multiple threads
    *   - Uses atomic operations to ensure unique ID generation without race conditions
    *   - Each position hash code gets its own counter, preventing ID conflicts
    *
    * @param position
    *   The position to generate a group ID for
    * @return
    *   A tuple (hashCode, uniqueId) where uniqueId is guaranteed to be unique for this position
    */
  private def getGrpId(position: Position): (Int, Int) =
    val hashCode = position.hashCode()
    val counter = positionCache.getOrElseUpdate(hashCode, new AtomicInteger(0))
    (hashCode, counter.getAndIncrement())

  def empty(eo: ElaborationOptions): DFC =
    DFC(None, Position.unknown, None, elaborationOptionsContr = () => eo)
  def emptyNoEO: DFC = DFC(None, Position.unknown, None)
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
