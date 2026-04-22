package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.annotation.Annotation

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
end DFC
object DFC:
  def emptyNoEO: DFC = ???
end DFC

into opaque type DFCG <: DFC = DFC
protected trait DFCGLP:
  inline given DFCG = DFCG()
object DFCG extends DFCGLP:
  def apply(): DFCG = DFC.emptyNoEO
  given DFCG(using dfc: DFC): DFCG = dfc
