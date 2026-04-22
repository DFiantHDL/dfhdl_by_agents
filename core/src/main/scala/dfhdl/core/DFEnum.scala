package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName

sealed abstract class DFEncoding extends scala.reflect.Enum

type DFEnum[E <: DFEncoding] = DFType[ir.DFEnum, Args1[E]]
object DFEnum:
  inline given [E <: DFEncoding]: DFEnum[E] = ???
  object Val:
    object TC
    object Compare
    object Ops
  end Val
end DFEnum
