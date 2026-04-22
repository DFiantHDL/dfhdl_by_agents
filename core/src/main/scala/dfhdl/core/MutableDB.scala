package dfhdl.core
import dfhdl.compiler.ir.MemberGetSet

final class MutableDB():
  val logger = new Logger
  given getSet: MemberGetSet = ???
end MutableDB
