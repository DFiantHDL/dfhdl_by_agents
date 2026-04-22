package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import compiletime.ops.int
import int.*
import compiletime.{constValueOpt, constValue}
import dfhdl.internals.Inlined
import scala.annotation.targetName

type IntP = Int | DFConstInt32
object IntP:
  type +[L <: IntP, R <: IntP] = Int
  type Max[L <: IntP, R <: IntP] = Int
end IntP

into opaque type IntParam[V <: IntP] = Int | DFConstInt32
object IntParam:
  inline implicit def fromValue[T <: IntP & Singleton](inline value: T): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
end IntParam
