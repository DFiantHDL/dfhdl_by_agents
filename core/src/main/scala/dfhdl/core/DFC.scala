package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound
import ir.annotation.HWAnnotation

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    mutableDB: MutableDB = new MutableDB()
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt,
      annotations: List[Annotation] = Nil
  ): this.type = ???
  def setMeta(meta: ir.Meta): this.type = ???
  def anonymize: this.type = ???
  def setName(name: String): this.type = ???
  given getSet: ir.MemberGetSet = mutableDB.getSet
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
    sealed trait Process extends Local
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
