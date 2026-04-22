package dfhdl.core
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import dfhdl.internals.*

trait DFMember[+T <: ir.DFMember] extends Any:
  val irValue: T | DFError

type DFMemberAny = DFMember[ir.DFMember]
object DFMember:
  extension [T <: ir.DFMember](member: DFMember[T])
    inline def asIR: T = ???
end DFMember

extension [M <: ir.DFMember](member: M)
  def addMember(using DFC): M = ???
  def replaceMemberWith(updated: M)(using DFC): M = ???
  def removeTagOf[CT <: ir.DFTag: ClassTag](using dfc: DFC): M = ???
end extension
