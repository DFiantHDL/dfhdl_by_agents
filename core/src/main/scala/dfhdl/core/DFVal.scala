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
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import scala.annotation.tailrec
import dfhdl.platforms.resources.Resource

import scala.reflect.ClassTag
into final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends DFMember[ir.DFVal]
    with Selectable:
  type Fields = DFVal.Fields[T @uncheckedVariance, M @uncheckedVariance]

  def wait(using DFC): Unit =
    trydf { Wait(this.asValOf[DFBoolOrBit]) }
  def selectDynamic(name: String)(using DFC): Any = trydf {
    val ir.DFStruct(structName, fieldMap) = this.asIR.dfType.runtimeChecked
    val dfType = fieldMap(name)
    DFVal.Alias
      .SelectField(this, name)
      .asIR
      .asVal[DFTypeAny, ModifierAny]
  }

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
  def isConstBool: Boolean =
    import quotes.reflect.*
    tpe.asType match
      case '[DFConstOf[t]]  => true
      case '[DFValOf[t]]    => false
      case '[NonEmptyTuple] =>
        tpe.getTupleArgs.forall(_.isConstBool)
      case '[SameElementsVector[t]]      => TypeRepr.of[t].isConstBool
      case '[BoolSelWrapper[sp, ot, of]] =>
        List(TypeRepr.of[sp], TypeRepr.of[ot], TypeRepr.of[of]).forall(_.isConstBool)
      case '[DFVal.NOTHING] => false
      case _                => true
  def isConstTpe: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    if (tpe.isConstBool) TypeRepr.of[CONST]
    else TypeRepr.of[NOTCONST]
end extension

inline def isConstCheck[T]: Boolean = ${ isConstCheckMacro[T] }
def isConstCheckMacro[T](using Quotes, Type[T]): Expr[Boolean] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  if (tpe.isConstBool) Expr(true)
  else Expr(false)

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def getNonConstTerm: Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case Apply(fun, args)    => (fun :: args).view.flatMap(_.getNonConstTerm).headOption
      case NamedArg(_, expr)   => expr.getNonConstTerm
      case Inlined(_, _, expr) => expr.getNonConstTerm
      case Block(_, expr)      => expr.getNonConstTerm
      case TypeApply(expr, _)  => expr.getNonConstTerm
      case Typed(expr, _)      => expr.getNonConstTerm
      case _                   =>
        term.tpe.asType match
          case '[DFConstOf[?]]  => None
          case '[DFValOf[?]]    => Some(term)
          case '[DFVal.NOTHING] => Some(term)
          case _                => None
    end match
  end getNonConstTerm
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
)(dfc: Expr[DFCG])(using Quotes, Type[T], Type[P], Type[R]): Expr[DFValTP[T, P]] =
  import quotes.reflect.*
  val fromExactInfo = from.exactInfo
  lazy val nonConstTermOpt = from.asTerm.getNonConstTerm
  if (TypeRepr.of[P] =:= TypeRepr.of[CONST] && nonConstTermOpt.nonEmpty)
    nonConstTermOpt.get.compiletimeErrorPosExpr("Applied argument must be a constant.")
  else
    val tStr = Expr(s"implicit conversion to type ${TypeRepr.of[T].showDFType}")
    '{
      val tc = compiletime.summonInline[DFVal.TCConv[T, fromExactInfo.Underlying]]
      trydf {
        tc(${ fromExactInfo.exactExpr })(using $dfc).asValTP[T, P]
      }(using $dfc, CTName($tStr))
    }
  end if
end DFValConversionMacro

sealed protected trait DFValLP:
  /* TODO: IMPORTANT IMPLICIT CONVERSION ISSUE
     -----------------------------------------
    Currently, the following issue affects the implicit conversion mechanism:
    https://github.com/lampepfl/dotty/issues/19388
    Because of this, we need to explicitly have a solid return type on the conversion definition
    to prevent returning `Nothing`. We have `CONST=:=ISCONST[true]` and `NOTCONST=:=Any`.
    For `NOTCONST`, what actually is happening is that the implicit conversion mechanism
    returns `ISCONST[Boolean]` and therefore we can detect it is not a constant. Because the
    modifier `M` of `DFVal[+T, +M]` is covariant, the compiler accepts
    `DFVal[T, NOTCONST] <:< DFVal[T, ISCONST[Boolean]]` and all is well.
   */
  type CommonR = DFValAny | Bubble | DFVal.NOTHING | BoolSelWrapper[?, ?, ?]

  implicit transparent inline def DFBitsValConversion[
      W <: IntP,
      P <: Boolean,
      R <: CommonR | SameElementsVector[?] | NonEmptyTuple
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFBits[W], ISCONST[P]] = ${
    DFValConversionMacro[DFBits[W], ISCONST[P], R]('from)('dfc)
  }
  // TODO: candidate should be fixed to cause UInt[?]->SInt[Int] conversion
  implicit transparent inline def DFXIntValConversion[
      S <: Boolean,
      W <: IntP,
      N <: NativeType,
      P <: Boolean,
      R <: CommonR | Int
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFXInt[S, W, N], ISCONST[P]] = ${
    DFValConversionMacro[DFXInt[S, W, N], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFOpaqueValConversion[
      TFE <: DFOpaque.Abstract,
      P <: Boolean,
      R <: CommonR
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFOpaque[TFE], ISCONST[P]] = ${
    DFValConversionMacro[DFOpaque[TFE], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFStructValConversion[
      F <: DFStruct.Fields,
      P <: Boolean,
      R <: CommonR | DFStruct.Fields
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFStruct[F], ISCONST[P]] = ${
    DFValConversionMacro[DFStruct[F], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFTupleValConversion[
      T <: NonEmptyTuple,
      P <: Boolean,
      R <: CommonR | NonEmptyTuple
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFTuple[T], ISCONST[P]] = ${
    DFValConversionMacro[DFTuple[T], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFVectorValConversion[
      T <: DFTypeAny,
      D <: IntP,
      P <: Boolean,
      R <: CommonR | Iterable[?] | SameElementsVector[?]
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFVector[T, Tuple1[D]], ISCONST[P]] = ${
    DFValConversionMacro[DFVector[T, Tuple1[D]], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFBitValConversion[
      P <: Boolean,
      R <: CommonR | Int | Boolean
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFBit, ISCONST[P]] = ${
    DFValConversionMacro[DFBit, ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFBoolValConversion[
      P <: Boolean,
      R <: CommonR | Int | Boolean
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFBool, ISCONST[P]] = ${
    DFValConversionMacro[DFBool, ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFEnumValConversion[
      E <: DFEncoding,
      P <: Boolean,
      R <: CommonR | E
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFEnum[E], ISCONST[P]] = ${
    DFValConversionMacro[DFEnum[E], ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFDoubleValConversion[
      P <: Boolean,
      R <: CommonR | Double
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFDouble, ISCONST[P]] = ${
    DFValConversionMacro[DFDouble, ISCONST[P], R]('from)('dfc)
  }
  implicit transparent inline def DFStringValConversion[
      P <: Boolean,
      R <: CommonR | String
  ](
      inline from: R
  )(using dfc: DFCG): DFValTP[DFString, ISCONST[P]] = ${
    DFValConversionMacro[DFString, ISCONST[P], R]('from)('dfc)
  }
  given DFUnitValConversion[R <: CommonR | Unit | NonEmptyTuple](using
      dfc: DFC
  ): Conversion[R, DFValOf[DFUnit]] = from => DFUnitVal().asInstanceOf[DFValOf[DFUnit]]
  given ConstToNonConstAccept[T <: DFTypeAny, P]: Conversion[DFValTP[T, P], DFValTP[T, NOTCONST]] =
    from => from.asValTP[T, NOTCONST]
  given DFRateToRateNumber(using
      dfc: DFCG
  ): Conversion[DFConstOf[DFTime | DFFreq], ir.RateNumber] =
    x =>
      import dfc.getSet
      x.asIR.getConstData[ir.RateNumber].toOption.get
  // lower priority than other evidence because this is more generic
  export DFXInt.Val.Ops.{evOpCommutativeArithDFXInt, evOpNonCommutativeArithDFXInt}
  export DFOpaque.Val.Ops.{evOpAsDFOpaqueTFE, evOpAsDFOpaqueComp}
  export DFBits.Val.Ops.{evLogicOpDFBits, evConcatOpDFBits}
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
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.asIR)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny     => Some(dfVal)
        case DFTuple.Val(dfVal)  => Some(dfVal)
        case DFStruct.Val(dfVal) => Some(dfVal)
        case _                   => None

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Int, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Boolean, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Tuple, DFVal[T, M]] =
    CanEqual.derived
  // Enabling encoding comparison
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[DFEncoding, DFVal[T, M]] =
    CanEqual.derived

  trait ConstCheck[P]
  given [P](using
      AssertGiven[
        P =:= CONST,
        "Only a DFHDL constant is convertible to a Scala value, but this DFHDL value is not a constant."
      ]
  ): ConstCheck[P] with {}

  extension [D, T <: ir.DFType, P](lhs: DFValTP[DFType[ir.DFType.Aux[T, Option[D]], ?], P])
    protected[core] def toScalaValue(using dfc: DFC, check: ConstCheck[P]): D = ???
  end extension

  extension [LW <: IntP, LT <: DFTypeW[LW]](lhs: DFValOf[LT])
    protected[core] def compareWidths[RW <: IntP, RT <: DFTypeW[RW]](
        rhs: DFValOf[RT]
    )(func: (Int, Int) => Boolean)(using dfc: DFC): Option[Boolean] = ???
  end extension

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a DFHDL port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
    @metaContextForward(0)
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): DFVal[T, M] = ???
    @metaContextForward(0)
    infix def tag[CT <: ir.DFTag: ClassTag](condCustomTag: Conditional[CT])(using dfc: DFC): DFVal[T, M] = ???
    def hasTag[CT <: ir.DFTag: ClassTag](using dfc: DFC): Boolean = ???
    @metaContextForward(0)
    infix def setName(name: String)(using dfc: DFC): DFVal[T, M] = ???
    def anonymize(using dfc: DFC): DFVal[T, M] = ???
    def inDFCPosition(using DFC): Boolean = ???
    def anonymizeInDFCPosition(using DFC): DFVal[T, M] = ???
    @metaContextForward(0)
    def nameInDFCPosition(using dfc: DFC): DFVal[T, M] = ???
  end extension

  @metaContextForward(0)
  trait InitValue[T <: DFTypeAny]:
    def enable: Boolean
    def apply(dfType: T)(using dfc: DFC): DFConstOf[T]
  object InitValue:
    transparent inline implicit def fromValue[T <: DFTypeAny, V](
        inline value: V
    ): InitValue[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: DFTypeAny, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[InitValue[T]] = ???
  end InitValue

  @metaContextForward(0)
  trait InitTupleValues[T <: NonEmptyTuple]:
    def enable: Boolean
    def apply(dfType: DFTuple[T])(using dfc: DFC): List[DFConstOf[DFTuple[T]]]
  object InitTupleValues:
    transparent inline implicit def fromValue[T <: NonEmptyTuple, V](
        inline value: V
    ): InitTupleValues[T] = ${ fromValueMacro[T, V]('value) }

    def fromValueMacro[T <: NonEmptyTuple, V](
        value: Expr[V]
    )(using Quotes, Type[T], Type[V]): Expr[InitTupleValues[T]] = ???
  end InitTupleValues

  extension [T <: DFTypeAny, A, C, I, P, R](dfVal: DFVal[T, Modifier[A, C, I, P]])
    private[dfhdl] def initForced(initValues: List[DFConstOf[T]])(using
        dfc: DFC
    ): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] = ???

    infix def init(
        initValues: InitValue[T]*
    )(using DFC, InitCheck[I]): DFVal[T, Modifier[A, C, Modifier.Initialized, P]] = ???
  end extension
  extension [T <: NonEmptyTuple, A, C, I, P](dfVal: DFVal[DFTuple[T], Modifier[A, C, I, P]])
    infix def init(
        initValues: InitTupleValues[T]
    )(using DFC, InitCheck[I]): DFVal[DFTuple[T], Modifier[A, C, Modifier.Initialized, P]] =
      trydf {
        if (initValues.enable)
          dfVal.initForced(initValues(dfVal.dfType)(using dfc.anonymize))
        else dfVal.initForced(Nil)
      }

  extension [W <: IntP, T <: DFBits[W] | DFUInt[W], D1 <: IntP, A, C, I, P](
      dfVal: DFVal[DFVector[T, Tuple1[D1]], Modifier[A, C, I, P]]
  )
    infix def initFile(
        path: String,
        format: ir.InitFileFormat = ir.InitFileFormat.Auto,
        undefinedValue: ir.InitFileUndefinedValue = ir.InitFileUndefinedValue.Zeros
    )(using
        dfc: DFC,
        check: InitCheck[I]
    ): DFVal[DFVector[T, Tuple1[D1]], Modifier[A, C, Modifier.Initialized, P]] = trydf:
      import dfc.getSet
      val vectorType = dfVal.dfType
      import DFVector.{lengthIntOpt, cellType}
      val length = vectorType.lengthIntOpt.getOrElse {
        throw new IllegalArgumentException(
          s"Vector length must be a known integer literal to be initialized from a file."
        )
      }
      val width = vectorType.cellType.widthIntOpt.getOrElse {
        throw new IllegalArgumentException(
          s"Vector cell type must have a known width to be initialized from a file."
        )
      }
      val data = ir.InitFileFormat.readInitFile(
        path, format, length, width, undefinedValue
      )
      val initFileConst = vectorType.cellType.asIR match
        case ir.DFBits(_) => DFVal.Const(vectorType, data)
        case cellType     =>
          DFVal.Const(vectorType, data.map(cellType.bitsDataToData))

      dfVal.initForced(List(initFileConst))
    // TODO: for now, we read the data immediately. In the future, incremental compilation will make
    // it beneficial to wait for the backend last stages to do so.
    // infix def initFile(
    //     path: String,
    //     format: ir.InitFileFormat = ir.InitFileFormat.Auto
    // )(using
    //     DFC,
    //     InitCheck[I]
    // ): DFVal[DFVector[DFBits[W], D], Modifier[A, C, Modifier.Initialized, P]] =
    //   val initFileFunc =
    //     DFVal.Func(dfVal.dfType, DFVal.Func.Op.InitFile(format, path), List.empty[ir.DFVal])(using
    //       dfc.anonymize
    //     ).asConstOf[DFVector[DFBits[W], D]]
    //   dfVal.initForced(List(initFileFunc))
  end extension

  implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
    ???

  // opaque values need special conversion that does not try to summon the opaque dftype
  // because it can be abstract in extension methods that are applied generically on an abstract
  // opaque super-type. E.g.:
  // ```
  // abstract class MyAbsOpaque extends Opaque
  // case class MyOpaque extends MyAbsOpaque
  // extension (a : MyAbsOpaque <> VAL) def foo : Unit = {}
  // val a = MyOpaque <> VAR
  // a.foo //here we currently access `foo` through conversion to MyAbsOpaque
  //       //because DFOpaque is not completely covariant due to bug
  //       //https://github.com/lampepfl/dotty/issues/15704
  // ```
  given DFOpaqueValConversion[T <: DFOpaque.Abstract, R <: DFOpaque.Abstract](using
      DFC,
      R <:< T
  ): Conversion[DFValOf[DFOpaque[R]], DFValOf[DFOpaque[T]]] = from =>
    from.asInstanceOf[DFValOf[DFOpaque[T]]]

  object Const:
    def apply[IRT <: ir.DFType, D, T <: DFType[ir.DFType.Aux[IRT, D], ?]](
        dfType: T, data: D, named: Boolean = false
    )(using DFC): DFConstOf[T] = ???
    def forced[T <: DFTypeAny](
        dfType: T, data: Any, named: Boolean = false
    )(using DFC): DFConstOf[T] = ???
  end Const

  object DesignParam:
    def apply[T <: DFTypeAny](
        appliedVal: DFValOf[T], defaultVal: Option[DFValOf[T]] = None
    )(using dfc: DFC): DFConstOf[T] = ???
  end DesignParam

  type CLK_FREQ = DFValOf[DFFreq]
  def CLK_FREQ(using DFC, RTDomainOnly): DFValOf[DFFreq] = ???

  type OPEN = OPEN.type
  object OPEN:
    protected[dfhdl] def apply[T <: DFTypeAny](dfType: T)(using DFC): DFValOf[T] = ???

  type NOTHING = NOTHING.type
  object NOTHING:
    protected[dfhdl] def apply[T <: DFTypeAny](dfType: T)(using DFC): DFValOf[T] = ???

  object Dcl:
    def apply[T <: DFTypeAny, M <: ModifierAny](
        dfType: T, modifier: M, initValues: List[DFConstOf[T]] = Nil
    )(using DFC): DFVal[T, M] = ???
    def iterator(using DFC): DFValOf[DFInt32] = ???
  end Dcl

  object Func:
    export ir.DFVal.Func.Op
    def apply[T <: DFTypeAny, P](
        dfType: T, op: FuncOp, args: List[DFValTP[?, P]]
    )(using DFC): DFValTP[T, P] = ???
    @scala.annotation.targetName("applyFromIR")
    def apply[T <: DFTypeAny, P](
        dfType: T, op: FuncOp, args: List[ir.DFVal]
    )(using dfc: DFC): DFValTP[T, P] = ???
  end Func

  object Alias:
    object AsIs:
      def apply[AT <: DFTypeAny, VT <: DFTypeAny, M <: ModifierAny](
          aliasType: AT,
          relVal: DFVal[VT, M],
          forceNewAlias: Boolean = false
      )(using dfc: DFC): DFVal[AT, M] = ???
      def ident[T <: DFTypeAny](relVal: DFVal[T, ?])(using DFC): Unit = ???
      def forced(aliasType: ir.DFType, relVal: ir.DFVal, forceNewAlias: Boolean = false)(using DFC): ir.DFVal = ???
    object History:
      def apply[T <: DFTypeAny, M <: ModifierAny](relVal: DFVal[T, M], step: Int, op: Any, initOption: Option[DFConstOf[T]])(using DFC): DFVal[T, M] = ???
    object ApplyRange:
      import IntP.{-, +}
      def apply[W <: IntP, M <: ModifierAny, H <: IntP, L <: IntP](
          relVal: DFVal[DFBits[W], M], idxHigh: IntParam[H], idxLow: IntParam[L]
      )(using DFC): DFVal[DFBits[H - L + 1], M] = ???
      def applyDFXInt[S <: Boolean, W <: IntP, M <: ModifierAny, H <: IntP, L <: IntP](
          relVal: DFVal[DFXInt[S, W, NativeType.BitAccurate], M], idxHigh: IntParam[H], idxLow: IntParam[L]
      )(using DFC): DFVal[DFXInt[S, H - L + 1, NativeType.BitAccurate], M] = ???
      def applyVector[T <: DFTypeAny, M <: ModifierAny, H <: IntP, L <: IntP](
          relVal: DFVal[DFVector[T, Tuple1[?]], M], idxHigh: IntParam[H], idxLow: IntParam[L]
      )(using DFC): DFVal[DFVector[T, Tuple1[H - L + 1]], M] = ???
      def forced[H <: IntP, L <: IntP](relVal: ir.DFVal, idxHigh: IntParam[H], idxLow: IntParam[L])(using DFC): ir.DFVal = ???
    object ApplyIdx:
      def apply[T <: DFTypeAny, W <: IntP, M <: ModifierAny](
          dfType: T, relVal: DFVal[DFTypeAny, M], relIdx: DFValOf[DFInt32]
      )(using DFC): DFVal[T, M] = ???
    object SelectField:
      def apply[T <: DFTypeAny, M <: ModifierAny](
          relVal: DFVal[DFTypeAny, M], fieldName: String
      )(using dfc: DFC): DFVal[T, M] = ???
  end Alias

  object PortByNameSelect:
    def apply(dfType: ir.DFType, designInst: ir.DFDesignBlock, namePath: String)(using DFC): ir.DFVal.PortByNameSelect = ???

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

  trait TCConvLP
  object TCConv extends TCConvLP:
    export DFBits.Val.TCConv.given
    export DFDecimal.Val.TCConv.given
    export DFTuple.Val.TCConv.given
    export DFVector.Val.TCConv.given

  trait TC_Or_OPEN_Or_Resource[T <: DFTypeAny, R] extends TC[T, R]:
    def connect(dfVal: DFValOf[T], that: R)(using DFC): Unit
  object TC_Or_OPEN_Or_Resource:
    type Exact[T <: DFTypeAny] = Exact1[DFTypeAny, T, [t <: DFTypeAny] =>> t, DFC, TC_Or_OPEN_Or_Resource]
  end TC_Or_OPEN_Or_Resource


  trait Compare[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean] extends TCCommon[T, V, DFValAny]:
    type OutP
    type Out = DFValTP[T, OutP]
    def apply[P](dfVal: DFValTP[T, P], arg: V)(using DFC, ValueOf[Op], ValueOf[C]): DFValTP[DFBool, P | OutP] = ???
  end Compare
  trait CompareLP
  object Compare extends CompareLP:
    type Aux[T <: DFTypeAny, V, Op <: FuncOp, C <: Boolean, OutP0] = Compare[T, V, Op, C] { type OutP = OutP0 }
    export DFBoolOrBit.Val.Compare.given
    export DFBits.Val.Compare.given
    export DFDecimal.Val.Compare.given
    export DFEnum.Val.Compare.given
    export DFVector.Val.Compare.given
    export DFTuple.Val.Compare.given
    export DFStruct.Val.Compare.given
    export TDFDouble.Val.Compare.given
    export TDFString.Val.Compare.given
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

  // exporting evidence for common exact operations
  export DFBits.Val.Ops.{
    evOpApplyDFBits,
    evOpApplyRangeDFBits,
    evOpAsDFBits,
    evOpLogicReduceDFBits,
    evOpShift
  }
  export DFBoolOrBit.Val.Ops.given
  export DFTuple.Val.Ops.given
  export DFVector.Val.Ops.given
  export DFXInt.Val.Ops.{
    evOpArithIntDFInt32,
    evOpApplyDFXInt,
    evOpApplyRangeDFXInt,
    evOpShiftOrPowerInt,
    evOpCarryAddSubDFXInt,
    evOpCarryMulDFXInt
  }
  export DFPhysical.Val.Ops.given
  export TDFDouble.Val.Ops.given
  export DFEnum.Val.Ops.given
  export DFOpaque.Val.Ops.{evOpAsDFOpaqueIterable, evOpClkAsClkComp, evOpRstAsRstComp}
  export TDFString.Val.Ops.given
  export ConnectOps.given

  given evOpCompare[LT <: DFTypeAny, LP, L <: DFValTP[LT, LP], R, Op <: FuncOp, RP](using
      tc: Compare.Aux[LT, R, Op, false, RP],
      op: ValueOf[Op]
  ): ExactOp2Aux[Op, DFC, DFValOf[DFBool], L, R, DFValTP[DFBool, LP | RP]] =
    new ExactOp2[Op, DFC, DFValOf[DFBool], L, R]:
      type Out = DFValTP[DFBool, LP | RP]
      def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
        tc(lhs, rhs)
      }(using dfc, CTName(op.value.toString))

  given evOpCompareCastled[L, LP, RT <: DFTypeAny, RP, R <: DFValTP[RT, RP], Op <: FuncOp](using
      tc: Compare.Aux[RT, L, Op, true, LP],
      op: ValueOf[Op]
  ): ExactOp2Aux[Op, DFC, DFValOf[DFBool], L, R, DFValTP[DFBool, LP | RP]] =
    new ExactOp2[Op, DFC, DFValOf[DFBool], L, R]:
      type Out = DFValTP[DFBool, LP | RP]
      def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
        tc(rhs, lhs)
      }(using dfc, CTName(op.value.toString))

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
  def assign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.Assignment, rhs.asIR)
  def nbassign[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    DFNet(dfVar.asIR, DFNet.Op.NBAssignment, rhs.asIR)

extension [T <: DFTypeAny](lhs: DFValOf[T])
  def connect[R <: DFTypeAny](rhs: DFValOf[R])(using DFC): Unit =
    val op = if (dfc.lateConstruction) DFNet.Op.ViaConnection else DFNet.Op.Connection
    DFNet(lhs.asIR, op, rhs.asIR)
end extension

trait VarsTuple[T <: NonEmptyTuple]:
  type Width <: Int
object VarsTuple:
  transparent inline given [T <: NonEmptyTuple]: VarsTuple[T] = ${ evMacro[T] }
  def evMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[VarsTuple[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    def varsCheck(tpe: TypeRepr): Option[String] =
      tpe.asTypeOf[Any] match
        case '[DFVarOf[t]]    => None
        case '[NonEmptyTuple] =>
          tpe.getTupleArgs.view.map(varsCheck).collectFirst { case Some(v) => v }
        case _ =>
          println(tpe.widen.dealias.show)
          Some(s"All tuple elements must be mutable but found an immutable type `${tpe.showType}`")
    varsCheck(tTpe) match
      case Some(err) => ControlledMacroError.report(err)
      case None      =>
        import Width.calcValWidth
        val widthType = tTpe.calcValWidth.asTypeOf[Int]
        '{
          new VarsTuple[T]:
            type Width = widthType.Underlying
        }
  end evMacro
end VarsTuple

final class REG_DIN[T <: DFTypeAny](val irValue: DFError.REG_DIN[T]) extends AnyVal:
  def :=(rhs: DFVal.TC.Exact[T])(using DFC): Unit = trydf {
    val dfVar = irValue.dfVar
    dfVar.assign(rhs(dfVar.dfType))
  }

object DFVarOps

object ConnectOps:
  def specialConnect[LT, LM, RT, RM](lhs: Any, rhs: Any)(using DFC): Unit = ???
end ConnectOps

extension (dfVal: ir.DFVal)
  protected[core] def isUnreachable(using dfc: DFC): Boolean =
    import dfc.getSet
    !dfVal.isGlobal && dfVal.getOwnerDesign.ownerRef != dfc.owner.asIR.getThisOrOwnerDesign.ownerRef
  private def cloneUnreachableWithNewRefs(using dfc: DFC): ir.DFVal =
    import dfc.getSet
    given ir.RefGen = dfc.refGen
    val currentOwner = dfc.owner.asIR
    val newDFVal = dfVal.copyWithNewRefs
    dfVal.getRefs.lazyZip(newDFVal.getRefs).foreach { case (oldRef, newRef) =>
      dfc.mutableDB.newRefFor(
        newRef,
        oldRef.get match
          case dfVal: ir.DFVal => dfVal.cloneUnreachable
          case m               => m
      )
    }
    dfc.mutableDB.newRefFor(newDFVal.ownerRef, currentOwner)
    dfc.mutableDB.addMember(newDFVal)
    newDFVal
  end cloneUnreachableWithNewRefs

  protected[core] def cloneUnreachable(using dfc: DFC): ir.DFVal =
    if (dfVal.isUnreachable)
      import dfc.getSet
      val currentDesign = dfc.owner.asIR.getThisOrOwnerDesign
      val dfValDesign = dfVal.getOwnerDesign
      // anonymous cloning
      if (dfVal.isAnonymous) cloneUnreachableWithNewRefs
      // named value cloning
      else
        dfc.mutableDB.DesignContext.getReachableNamedValue(
          dfVal,
          // when the current design is within the value's owner design,
          // create a design parameter to propagate the named value into the design.
          if (currentDesign.isInsideOwner(dfValDesign))
            DFVal.DesignParam(dfVal.asValAny)(using dfc.setMeta(dfVal.meta)).asIR
          // when the current design is outside the value's owner design
          else
            dfVal match
              // design parameter, so recurse on the referenced value
              case dp: ir.DFVal.DesignParam =>
                dp.appliedOrDefaultVal.cloneUnreachable
              // named constant, so clone under a new name within relation to the current design
              case _ =>
                val newMeta =
                  dfVal.meta.setName(dfVal.getRelativeName(currentDesign).replaceAll("\\.", "_"))
                dfVal.cloneUnreachableWithNewRefs.setMeta(_ => newMeta)
        )
      end if
    // reachable value, so no need to clone
    else dfVal
    end if
  end cloneUnreachable

  protected[dfhdl] def cloneAnonValueAndDepsHere(using dfc: DFC): ir.DFVal =
    import dfc.getSet
    if (dfVal.isAnonymous)
      val dfcForClone = dfc.setMeta(dfVal.meta).setTags(dfVal.tags)
      val dfType = dfVal.dfType.asFE[DFTypeAny]
      val cloned = dfVal match
        case const: ir.DFVal.Const =>
          DFVal.Const.forced(dfType, const.data)(using dfcForClone)
        case func: ir.DFVal.Func =>
          val clonedArgs = func.args.map(_.get.cloneAnonValueAndDepsHere)
          DFVal.Func(dfType, func.op, clonedArgs)(using dfcForClone)
        case alias: ir.DFVal.Alias.Partial =>
          val clonedRelValIR = alias.relValRef.get.cloneAnonValueAndDepsHere
          val clonedRelVal = clonedRelValIR.asValAny
          alias match
            case alias: ir.DFVal.Alias.AsIs =>
              DFVal.Alias.AsIs(dfType, clonedRelVal, forceNewAlias = true)(using dfcForClone)
            case alias: ir.DFVal.Alias.ApplyRange =>
              def cloneIntParam(intParam: IntParam[Int]): IntParam[Int] =
                val ret = intParam.runtimeChecked match
                  case int: Int                       => int
                  case const: DFConstInt32 @unchecked => const.asIR.cloneAnonValueAndDepsHere
                ret.asInstanceOf[IntParam[Int]]
              DFVal.Alias.ApplyRange(
                clonedRelVal.asValOf[DFBits[Int]], // this is OK even for UInt/SInt
                cloneIntParam(alias.idxHighRef.get),
                cloneIntParam(alias.idxLowRef.get)
              )(using dfcForClone)
            case alias: ir.DFVal.Alias.ApplyIdx =>
              val clonedIdx = alias.relIdx.get.cloneAnonValueAndDepsHere.asValOf[DFInt32]
              DFVal.Alias.ApplyIdx(dfType, clonedRelVal, clonedIdx)(using dfcForClone)
            case alias: ir.DFVal.Alias.SelectField =>
              DFVal.Alias.SelectField(clonedRelVal, alias.fieldName)(using dfcForClone)
          end match
        case pbns: ir.DFVal.PortByNameSelect =>
          DFVal.PortByNameSelect(pbns.dfType, pbns.designInstRef.get, pbns.portNamePath)(using
            dfcForClone
          ).asValAny
        case _ =>
          throw new IllegalArgumentException(s"Unsupported cloning for: $dfVal")
      cloned.asIR
    else dfVal
    end if
  end cloneAnonValueAndDepsHere
end extension
