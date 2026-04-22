package dfhdl.core
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import dfhdl.internals.*

trait DFMember[+T <: ir.DFMember] extends Any:
  val irValue: T | DFError
  override def toString: String = irValue.toString

type DFMemberAny = DFMember[ir.DFMember]
object DFMember:
  extension [T <: ir.DFMember](member: DFMember[T])
    inline def asIR: T = member.irValue.runtimeChecked match
      case memberIR: T @unchecked                   => memberIR
      case err: DFError.REG_DIN[?] if err.firstTime =>
        err.firstTime = false
        throw err
      case err: DFError => throw DFError.Derived(err)
end DFMember

extension [M <: ir.DFMember](member: M)
  def addMember(using DFC): M = ???
  def replaceMemberWith(updated: M)(using DFC): M = ???
  def removeTagOf[CT <: ir.DFTag: ClassTag](using dfc: DFC): M = ???
end extension
