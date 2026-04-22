package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import ir.DFVal.Alias.History.Op as HistoryOp
import ir.DFDecimal.NativeType

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import dfhdl.compiler.ir.MemberGetSet
import scala.annotation.tailrec

import scala.reflect.ClassTag
into final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal)
end DFVal

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, Modifier.Mutable]
type DFDclAny = DFVal[DFTypeAny, Modifier.Dcl]
type DFConstAny = DFVal[DFTypeAny, Modifier.CONST]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[Any, Any, Any, P]]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier.Mutable]

inline def isConstCheck[T]: Boolean = ${ isConstCheckMacro[T] }
def isConstCheckMacro[T](using Quotes, Type[T]): Expr[Boolean] = ???

type JUSTVAL[T] = DFValOf[DFTypeAny]


object DFVal:
  inline def apply[T <: DFTypeAny, M <: ModifierAny, IR <: ir.DFVal](
      irValue: IR
  ): DFVal[T, M] = new DFVal[T, M](irValue)

  export DFXInt.Val.Ops.{
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }

  object Const:
    def apply(args: Any*)(using DFC): DFConstOf[DFTypeAny] = ???

  trait TC[T <: DFTypeAny, R]:
    type OutP

  object Ops:
    protected[core] trait BoolOnlyOp[Op <: FuncOp]
    protected[core] trait CarryOp[Op <: FuncOp]
  end Ops
end DFVal


