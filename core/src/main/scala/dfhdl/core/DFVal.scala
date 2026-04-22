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

extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def isConstBool: Boolean = ???
  def isConstTpe: quotes.reflect.TypeRepr = ???
end extension

inline def isConstCheck[T]: Boolean = ${ isConstCheckMacro[T] }
def isConstCheckMacro[T](using Quotes, Type[T]): Expr[Boolean] = ???

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def getNonConstTerm: Option[quotes.reflect.Term] = ???
end extension

infix type <>[T <: DFType.Supported, M] = M match
  case DFRET => (DFC, DomainType.DF) ?=> DFValOf[DFType.Of[T]]
  case RTRET => (DFC, DomainType.RT) ?=> DFValOf[DFType.Of[T]]
  case EDRET => (DFC, DomainType.ED) ?=> DFValOf[DFType.Of[T]]
  case VAL   => DFValOf[DFType.Of[T]]
  case CONST => DFConstOf[DFType.Of[T]]

infix type X[T <: DFType.Supported, M] = DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension [V <: ir.DFVal](dfVal: V)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M, V](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny, V](dfVal)
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] =
    DFVal[T, Modifier[Any, Any, Any, P], V](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny, V](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.Mutable, V](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.Mutable, V](dfVal)
  inline def asDclAny: DFDclAny =
    DFVal[DFTypeAny, Modifier.Dcl, V](dfVal)
  inline def asConstAny: DFConstOf[DFTypeAny] =
    DFVal[DFTypeAny, Modifier.CONST, V](dfVal)
  inline def asConstOf[T <: DFTypeAny]: DFConstOf[T] =
    DFVal[T, Modifier.CONST, V](dfVal)
end extension

extension (dfVal: DFValAny)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    dfVal.asInstanceOf[DFVal[T, ModifierAny]]
  inline def asValTP[T <: DFTypeAny, P]: DFValTP[T, P] =
    dfVal.asInstanceOf[DFVal[T, Modifier[Any, Any, Any, P]]]
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    dfVal.asInstanceOf[DFVal[T, Modifier.Mutable]]
  inline def asVarAny: DFVarAny =
    dfVal.asInstanceOf[DFVal[DFTypeAny, Modifier.Mutable]]
  inline def asDclAny: DFDclAny =
    dfVal.asInstanceOf[DFVal[DFTypeAny, Modifier.Dcl]]
  inline def asConstOf[T <: DFTypeAny]: DFConstOf[T] =
    dfVal.asInstanceOf[DFVal[T, Modifier.CONST]]
end extension

def DFValConversionMacro[T <: DFTypeAny, P, R](
    from: Expr[R]
)(dfc: Expr[DFCG])(using Quotes, Type[T], Type[P], Type[R]): Expr[DFValTP[T, P]] = ???

sealed protected trait DFValLP:
  type CommonR = DFValAny | Bubble | DFVal.NOTHING | BoolSelWrapper[?, ?, ?]
end DFValLP
object DFVal extends DFValLP:
  protected type FieldWithModifier[V, M <: ModifierAny] = V match
    case DFVal[t, _] =>
      M match
        case Modifier[a, Any, i, p] => DFVal[t, Modifier[a, Any, i, p]]
  protected type FieldsWithModifier[V <: NamedTuple.AnyNamedTuple, M <: ModifierAny] =
    NamedTuple.Map[V, [t] =>> FieldWithModifier[t, M]]
  protected[core] type Fields[T <: DFTypeAny, M <: ModifierAny] = T match
    case DFType[t, Args1[a]] =>
      t match
        case ir.DFStruct => FieldsWithModifier[NamedTuple.From[a], M]
        case _           => Any
    case _ => Any

  // constructing a front-end DFVal value class object. if it's a global value, then
  // we need to save the DFC, instead of the actual member IR object
  inline def apply[T <: DFTypeAny, M <: ModifierAny, IR <: ir.DFVal | DFError](
      irValue: IR
  ): DFVal[T, M] = new DFVal[T, M](irValue)
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = ???

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

  trait TCLP
  object TC extends TCLP:
    type Exact[T <: DFTypeAny] = Exact1[DFTypeAny, T, [t <: DFTypeAny] =>> t, DFC, TC]
    type Aux[T <: DFTypeAny, R, OutP0] = TC[T, R] { type OutP = OutP0 }
    export DFBoolOrBit.Val.TC.given
    export DFBits.Val.TC.given
    export DFDecimal.Val.TC.given
    export DFEnum.Val.TC.given
    export DFVector.Val.TC.given
    export DFTuple.Val.TC.given
    export DFStruct.Val.TC.given
    export DFOpaque.Val.TC.given
    export TDFDouble.Val.TC.given
    export TDFString.Val.TC.given
  end TC

  trait TCConv[T <: DFTypeAny, R] extends TC[T, R]:
    type OutP
    type Out = DFValTP[T, OutP]
    def conv(dfType: T, from: R)(using DFC): Out = ???
    def apply(from: R)(using DFC): Out

  object TCConv:
    export DFBits.Val.TCConv.given
    export DFDecimal.Val.TCConv.given

  trait Compare[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean] extends TCCommon[T, V, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
  end Compare
  object Compare:
    type Aux[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean, OutP0] = Compare[T, V, Op, C] { type OutP = OutP0 }
    export DFBoolOrBit.Val.Compare.given
    export DFBits.Val.Compare.given
    export DFDecimal.Val.Compare.given
    export DFEnum.Val.Compare.given
  end Compare

  trait DFDomainOnly
  given (using
      domain: DomainType
  )(using
      AssertGiven[
        domain.type <:< DomainType.DF,
        "This construct is only available in a dataflow domain."
      ]
  ): DFDomainOnly with {}
  trait RTDomainOnly
  given (using
      domain: DomainType
  )(using
      AssertGiven[
        domain.type <:< DomainType.RT,
        "This construct is only available in a register-transfer domain."
      ]
  ): RTDomainOnly with {}
  trait PrevInitCheck[I]
  given [I](using
      AssertGiven[
        I =:= Modifier.Initialized,
        "Value must be an initialized declaration or `.prev` must have an initialization argument.\nE.g.: `x.prev(step, init)`.\nIt's possible to apply a bubble initialization with `init = ?`"
      ]
  ): PrevInitCheck[I] with {}
  trait RegInitCheck[I]
  given [I](using
      AssertGiven[
        I =:= Modifier.Initialized,
        "Value must be an initialized declaration or `.reg` must have an initialization argument.\nE.g.: `x.reg(step, init)`.\nIt's possible to apply an unknown initialization with `init = ?`"
      ]
  ): RegInitCheck[I] with {}

  export DFXInt.Val.Ops.{
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }

  object Ops:
    protected type SupportedValue =
      DFValAny | Boolean | Int | Long | Double | NonEmptyTuple | Iterable[DFValAny] |
        SameElementsVector[?] | BoolSelWrapper[?, ?, ?]
    extension (inline lhs: DFValAny)
      transparent inline def apply(inline idx: Any)(using DFCG): DFValAny = ???
      transparent inline def apply(inline idxLeft: Any, inline idxRight: Any)(using DFCG): DFValAny = ???
    end extension
    protected[core] trait BoolOnlyOp[Op <: FuncOp]
    protected[core] trait CarryOp[Op <: FuncOp]
    private[core] transparent inline def compare[Op <: FuncOp, L, R](
        inline lhs: L, inline rhs: R
    )(using DFC, ValueOf[Op]): DFValOf[DFBool] = ???
    extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
      def bits(using DFCG)(using w: Width[T]): DFValTP[DFBits[w.Out], P] = ???
      def genNewVar(using DFC): DFVarOf[T] = ???
    end extension
  end Ops
end DFVal


extension [T <: DFTypeAny](dfVar: DFValOf[T])
  def assign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit = ???
  def nbassign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit = ???

extension [T <: DFTypeAny](lhs: DFValOf[T])
  def connect[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit = ???
end extension

trait VarsTuple[T <: NonEmptyTuple]:
  type Width <: Int
object VarsTuple:
  transparent inline given [T <: NonEmptyTuple]: VarsTuple[T] = ${ evMacro[T] }
  def evMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[VarsTuple[T]] = ???
end VarsTuple

final class REG_DIN[T <: DFTypeAny](val irValue: DFError.REG_DIN[T]) extends AnyVal:
  def :=(rhs: DFVal.TC.Exact[T])(using DFC): Unit = ???

object DFVarOps

object ConnectOps:
  def specialConnect[LT, LM, RT, RM](lhs: Any, rhs: Any)(using DFC): Unit = ???
end ConnectOps

extension (dfVal: ir.DFVal)
  protected[core] def isUnreachable(using dfc: DFC): Boolean = ???
  protected[core] def cloneUnreachable(using dfc: DFC): ir.DFVal = ???
  protected[dfhdl] def cloneAnonValueAndDepsHere(using dfc: DFC): ir.DFVal = ???
end extension
