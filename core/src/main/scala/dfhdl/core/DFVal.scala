package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import ir.DFVal.Alias.History.Op as HistoryOp
import ir.DFDecimal.NativeType

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import dfhdl.compiler.ir.MemberGetSet
import scala.annotation.tailrec

import scala.reflect.ClassTag
into final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends DFMember[ir.DFVal]
    with Selectable:
  type Fields = DFVal.Fields[T @uncheckedVariance, M @uncheckedVariance]

  def wait(using DFC): Unit = ???
  def selectDynamic(name: String)(using DFC): Any = ???

  transparent inline def ==[R](
      inline that: R
  )(using DFCG): DFValOf[DFBool] =
    DFVal.Ops.compare[FuncOp.===.type, this.type, R](this, that)

  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFValTP[DFBool, Any] =
    DFVal.Ops.compare[FuncOp.=!=.type, this.type, R](this, that)
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

infix type <>[T <: DFType.Supported, M] = M match
  case DFRET => (DFC, DomainType.DF) ?=> DFValOf[DFType.Of[T]]
  case RTRET => (DFC, DomainType.RT) ?=> DFValOf[DFType.Of[T]]
  case EDRET => (DFC, DomainType.ED) ?=> DFValOf[DFType.Of[T]]
  case VAL   => DFValOf[DFType.Of[T]]
  case CONST => DFConstOf[DFType.Of[T]]

infix type X[T <: DFType.Supported, M] = DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension [V <: ir.DFVal](dfVal: V)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] = ???
end extension

extension (dfVal: DFValAny)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] = ???
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] = ???
end extension

def DFValConversionMacro[T <: DFTypeAny, P, R](
    from: Expr[R]
)(dfc: Expr[DFCG])(using Quotes, Type[T], Type[P], Type[R]): Expr[DFValTP[T, P]] = ???

object DFVal:
  protected[core] type Fields[T <: DFTypeAny, M <: ModifierAny] = Any

  inline def apply[T <: DFTypeAny, M <: ModifierAny, IR <: ir.DFVal | DFError](
      irValue: IR
  ): DFVal[T, M] = new DFVal[T, M](irValue)

  trait ConstCheck[P]

  extension [D, T <: ir.DFType, P](lhs: DFValTP[DFType[ir.DFType.Aux[T, Option[D]], ?], P])
    protected[core] def toScalaValue(using dfc: DFC, check: ConstCheck[P]): D = ???
  end extension

  object Const:
    def apply[IRT <: ir.DFType, D, T <: DFType[ir.DFType.Aux[IRT, D], ?]](
        dfType: T, data: D, named: Boolean = false
    )(using DFC): DFConstOf[T] = ???
  end Const

  type OPEN = OPEN.type
  object OPEN

  type NOTHING = NOTHING.type
  object NOTHING

  object Func:
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny, P](
        dfType: T, op: FuncOp, args: List[DFValTP[?, P]]
    )(using DFC): DFValTP[T, P] = ???
  end Func

  type CLK_FREQ = DFValOf[DFFreq]

  trait TC[T <: DFTypeAny, R] extends TCCommon[T, R, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
    final def apply(dfType: T, value: R)(using DFC): Out = ???

  object TCDummy extends TC[DFTypeAny, DFValOf[DFTypeAny]]:
    type OutP = NOTCONST
    def conv(dfType: DFTypeAny, value: DFValOf[DFTypeAny])(using dfc: DFC): DFValOf[DFTypeAny] = ???

  object TC:
    type Exact[T <: DFTypeAny] = Exact1[DFTypeAny, T, [t <: DFTypeAny] =>> t, DFC, TC]
    type Aux[T <: DFTypeAny, R, OutP0] = TC[T, R] { type OutP = OutP0 }
  end TC

  trait TCConv[T <: DFTypeAny, R] extends TC[T, R]:
    type OutP
    type Out = DFValTP[T, OutP]
    def conv(dfType: T, from: R)(using DFC): Out = ???
    def apply(from: R)(using DFC): Out

  object TCConv

  trait Compare[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean] extends TCCommon[T, V, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
  end Compare
  object Compare:
    type Aux[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean, OutP0] = Compare[T, V, Op, C] { type OutP = OutP0 }
  end Compare

  trait DFDomainOnly
  trait RTDomainOnly
  trait PrevInitCheck[I]
  trait RegInitCheck[I]

  export DFXInt.Val.Ops.{
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }

  object Ops:
    protected[core] trait BoolOnlyOp[Op <: FuncOp]
    protected[core] trait CarryOp[Op <: FuncOp]
    private[core] transparent inline def compare[Op <: FuncOp, L, R](
        inline lhs: L, inline rhs: R
    )(using DFC, ValueOf[Op]): DFValOf[DFBool] = ???
  end Ops
end DFVal


final class REG_DIN[T <: DFTypeAny](val irValue: DFError.REG_DIN[T]) extends AnyVal
