package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.*

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: IntP, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: IntP, F <: Int, N <: NativeType](
      signed: Inlined[S],
      width: IntParam[W],
      fractionWidth: Inlined[F],
      nativeType: N
  )(using dfc: DFC, check: Width.CheckNUB[S, W]): DFDecimal[S, W, F, N] = ???
  protected[core] def forced[S <: Boolean, W <: IntP, F <: Int, N <: NativeType](
      signed: Boolean,
      width: Int,
      fractionWidth: Int,
      nativeType: NativeType
  )(using DFC): DFDecimal[S, W, F, N] = ???

  given DFInt32 = DFInt32
  given [S <: Boolean, W <: IntP & Singleton, F <: Int, N <: NativeType](using
      ValueOf[S], ValueOf[W], ValueOf[F], ValueOf[N]
  )(using DFCG, Width.CheckNUB[S, W]): DFDecimal[S, W, F, N] = ???
  object Extensions:
    extension [S <: Boolean, W <: IntP, F <: Int, N <: NativeType](dfType: DFDecimal[S, W, F, N])
      def signed: Inlined[S] = ???
      def nativeType: N = ???

  protected[core] object Constraints:
    object Width extends Check2[Boolean, Int, [s <: Boolean, w <: Int] =>> true, [s <: Boolean, w <: Int] =>> ""]
    object Sign extends Check2[Boolean, Int, [s <: Boolean, n <: Int] =>> true, [s <: Boolean, n <: Int] =>> ""]
    object `LW >= RW` extends Check2[Int, Int, [LW <: Int, RW <: Int] =>> true, [LW <: Int, RW <: Int] =>> ""]
    object `W <= 32` extends Check1[Int, [W <: Int] =>> true, [W <: Int] =>> ""]
    object `W <= 31` extends Check1[Int, [W <: Int] =>> true, [W <: Int] =>> ""]
    object `LW == RW` extends Check2[Int, Int, [LW <: Int, RW <: Int] =>> true, [LW <: Int, RW <: Int] =>> ""]
    object `LS >= RS` extends Check2[Boolean, Boolean, [LS <: Boolean, RS <: Boolean] =>> true, [LS <: Boolean, RS <: Boolean] =>> ""]
    object `BaS >= WcS` extends Check2[Boolean, Boolean, [BaS <: Boolean, WcS <: Boolean] =>> true, [BaS <: Boolean, WcS <: Boolean] =>> ""]
    object `BaW >= WcW` extends Check2[Int, Int, [BaW <: Int, WcW <: Int] =>> true, [BaW <: Int, WcW <: Int] =>> ""]
    object `LS == RS` extends Check2[Boolean, Boolean, [LS <: Boolean, RS <: Boolean] =>> true, [LS <: Boolean, RS <: Boolean] =>> ""]
    trait TCCheck[LS <: Boolean, LW <: IntP, RS <: Boolean, RW <: IntP]:
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit
    given [LS <: Boolean, LW <: IntP, RS <: Boolean, RW <: IntP]: TCCheck[LS, LW, RS, RW] with
      def apply(leftSigned: Boolean, leftWidth: Int, rightSigned: Boolean, rightWidth: Int): Unit = ???
    trait CompareCheck[
        ValS <: Boolean,
        ValW <: IntP,
        ArgS <: Boolean,
        ArgW <: IntP,
        ArgIsInt <: Boolean, // argument is a wildcard (Int32 NativeType)
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit
    end CompareCheck
    given [ValS <: Boolean, ValW <: IntP, ArgS <: Boolean, ArgW <: IntP, ArgIsInt <: Boolean, Castle <: Boolean]
        : CompareCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(dfValSigned: Boolean, dfValWidth: Int, argSigned: Boolean, argWidth: Int): Unit = ???

    trait ArithCheck[
        LS <: Boolean,
        LW <: IntP,
        LN <: NativeType,
        RS <: Boolean,
        RW <: IntP,
        RN <: NativeType
    ]:
      def apply(
          lhs: DFValOf[DFXInt[LS, LW, LN]],
          rhs: DFValOf[DFXInt[RS, RW, RN]]
      )(using DFC): Unit
    end ArithCheck
    given [LS <: Boolean, LW <: IntP, LN <: NativeType, RS <: Boolean, RW <: IntP, RN <: NativeType]
        : ArithCheck[LS, LW, LN, RS, RW, RN] with
      def apply(lhs: DFValOf[DFXInt[LS, LW, LN]], rhs: DFValOf[DFXInt[RS, RW, RN]])(using dfc: DFC): Unit = ???

    trait SignCheck[
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean, // argument is a wildcard (Int32 NativeType)
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          argSigned: Boolean
      ): Unit
    given [ValS <: Boolean, ArgS <: Boolean, ArgIsInt <: Boolean, Castle <: Boolean]
        : SignCheck[ValS, ArgS, ArgIsInt, Castle] with
      def apply(dfValSigned: Boolean, argSigned: Boolean): Unit = ???

    type NativeCheck[LN <: NativeType, RN <: NativeType] =
      AssertGiven[
        (RN =:= Int32) | ((LN =:= RN) | (LN =:= BitAccurate)),
        "Cannot implicitly convert to DFHDL Int type."
      ]
  end Constraints

  object StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.*
    opaque type DecStrCtx <: StringContext = StringContext
    object DecStrCtx:
      extension (inline sc: DecStrCtx)
        transparent inline def apply(inline args: Any*)(using dfc: DFCG): Any =
          ${ applyMacro('sc, 'args)('dfc) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using dfc: DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('sc, 'arg)('dfc) }

    extension (sc: StringContext)
      def d: DecStrCtx = sc
      def sd: DecStrCtx = sc
    end extension

    private def uintConst(value: BigInt)(using DFC): DFConstOf[DFUInt[Int]] = ???
    private def sintConst(value: BigInt)(using DFC): DFConstOf[DFSInt[Int]] = ???

    private def applyMacro(
        sc: Expr[DecStrCtx],
        args: Expr[Seq[Any]]
    )(dfc: Expr[DFC])(using Quotes): Expr[DFConstAny] = ???

    private def unapplySeqMacro[T <: DFTypeAny](
        sc: Expr[DecStrCtx],
        arg: Expr[DFValOf[T]]
    )(dfc: Expr[DFC])(using Quotes, Type[T]): Expr[Option[Seq[DFValOf[T]]]] = ???
  end StrInterpOps

  object Val:
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int, NativeType],
          dfVal: DFValOf[DFDecimal[Boolean, Int, Int, NativeType]]
      )(using DFC): DFValOf[DFDecimal[Boolean, Int, Int, NativeType]] = ???
    end TC
    object TCConv:
      export DFXInt.Val.TCConv.given
    object Compare:
      export DFXInt.Val.Compare.given
    object Ops:
      export DFXInt.Val.Ops.*
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: IntP, N <: NativeType] = DFDecimal[S, W, 0, N]
object DFXInt:
  def apply[S <: Boolean, W <: IntP, N <: NativeType & Singleton](
      signed: Inlined[S],
      width: IntParam[W],
      nativeType: N
  )(using DFC, Width.CheckNUB[S, W]): DFXInt[S, W, N] = DFDecimal(signed, width, 0, nativeType)

  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutS <: Boolean
      type OutW <: IntP
      type OutN <: NativeType
      type OutP
      type Out = DFValTP[DFXInt[OutS, OutW, OutN], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out
    trait CandidateLP
    object Candidate extends CandidateLP:
      type Aux[R, S <: Boolean, W <: IntP, N <: NativeType, P] =
        Candidate[R] {
          type OutS = S
          type OutW = W
          type OutN = N
          type OutP = P
        }
    end Candidate

    object TC
    object TCConv
    object Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      import IntP.{-, +}

      export dfhdl.internals.clog2
      def clog2[P, S <: Boolean, W <: IntP, N <: NativeType](
          dfVal: DFValTP[DFXInt[S, W, N], P]
      )(using DFCG, DFVal.ConstCheck[P]): DFValTP[DFXInt[S, W, N], P] = ???
      def abs[P, S <: Boolean, W <: IntP, N <: NativeType](
          dfVal: DFValTP[DFXInt[S, W, N], P]
      )(using DFCG): DFValTP[DFXInt[S, W, N], P] = ???
      extension [P, S <: Boolean, W <: IntP, N <: NativeType](lhs: DFValTP[DFXInt[S, W, N], P])
        protected[core] def toDFXIntOf[RS <: Boolean, RW <: IntP, RN <: NativeType](
            dfType: DFXInt[RS, RW, RN]
        )(using dfc: DFC): DFValTP[DFXInt[RS, RW, RN], P] = ???
        def toScalaInt(using DFC, DFVal.ConstCheck[P]): Int = ???
        def toScalaBigInt(using DFC, DFVal.ConstCheck[P]): BigInt = ???
      end extension
      extension [S <: Boolean, W <: IntP, N <: NativeType, P](lhs: DFValTP[DFXInt[S, W, N], P])
        @targetName("resizeDFXIntAuto")
        def resize(using DFCG): DFValTP[DFXInt[S, Int, N], P] = ???
        @targetName("resizeDFXInt")
        def resize[RW <: IntP](
            updatedWidth: IntParam[RW]
        )(using
            dfc: DFCG,
            check: Width.CheckNUB[S, RW]
        ): DFValTP[DFXInt[S, RW, BitAccurate], P] = ???
        end resize
      end extension

      private[core] val verilogSemanticsWarnMsg = ""
      private[core] def hasImplicitlyFromIntTag(dfVal: ir.DFVal): Boolean = ???
      private[core] def containsNarrowNonCarryArith(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean = ???
      private[core] def containsNarrowNonCarryArithWithTaggedOperand(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean = ???

      // Check that a wildcard `Int` value fits in the bit-accurate value's type.
      // Produces an elaboration error if it doesn't.
      private def checkWildcardFit(
          wildcard: DFValOf[DFInt32],
          bitAccurateType: DFTypeAny
      )(using dfc: DFC): Unit = ???

      private def arithOp[
          OS <: Boolean,
          OW <: IntP,
          ON <: NativeType,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](
          dfType: DFXInt[OS, OW, ON],
          op: FuncOp,
          lhs: DFValTP[DFXInt[LS, LW, LN], LP],
          rhs: DFValTP[DFXInt[RS, RW, RN], RP]
      )(using dfc: DFC): DFValTP[DFXInt[OS, OW, ON], LP | RP] = ???

      type CommutativeArithOp =
        FuncOp.+.type | FuncOp.*.type | FuncOp.max.type | FuncOp.min.type
      type NonCommutativeArithOp =
        FuncOp.-.type | FuncOp./.type | FuncOp.%.type
      type ArithOp = CommutativeArithOp | NonCommutativeArithOp
      given evOpArithIntDFInt32[
          Op <: ArithOp,
          L <: Int,
          RP,
          R <: DFValTP[DFInt32, RP]
      ](using
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFInt32, RP]] =
        ???
      end evOpArithIntDFInt32
      given evOpCommutativeArithDFXInt[
          Op <: CommutativeArithOp, L, LS <: Boolean, LW <: IntP, LN <: NativeType, LP,
          R, RS <: Boolean, RW <: IntP, RN <: NativeType, RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???
      given evOpNonCommutativeArithDFXInt[
          Op <: NonCommutativeArithOp, L, LS <: Boolean, LW <: IntP, LN <: NativeType, LP,
          R, RS <: Boolean, RW <: IntP, RN <: NativeType, RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFXInt[Boolean, Int, NativeType], LP | RP]] = ???

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[
          Op <: FuncOp.+.type | FuncOp.-.type,
          L, LS <: Boolean, LW <: IntP, LN <: NativeType, LP,
          R, RS <: Boolean, RW <: IntP, RN <: NativeType, RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[
        DFXInt[LS || RS, IntP.+[IntP.Max[LW, RW], 1], BitAccurate],
        LP | RP
      ]] = ???

      given evOpCarryMulDFXInt[
          Op <: FuncOp.`*`.type,
          L, LS <: Boolean, LW <: IntP, LN <: NativeType, LP,
          R, RS <: Boolean, RW <: IntP, RN <: NativeType, RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[
        DFXInt[LS || RS, IntP.+[LW, RW], BitAccurate],
        LP | RP
      ]] = ???

      // TODO: this takes the RHS's width as the result type width. This is how VHDL behaves.
      // But verilog requires the result type width to be the same as the LHS's width.
      // The general rule that we apply in evOpArithDFXInt is to take the LHS's width and the RHS is also resized to the LHS's width.
      // This approach always works, but then requires resizing if we require the actual (smaller) width of the result.
      // However when compiling to verilog this creates linting warnings.
    end Ops
  end Val
end DFXInt

type DFUInt[W <: IntP] = DFXInt[false, W, BitAccurate]
object DFUInt:
  def apply[W <: IntP](width: IntParam[W])(using DFCG, Width.CheckNUB[false, W]): DFUInt[W] = ???
  def forced[W <: IntP](width: IntP)(using DFC): DFUInt[W] = ???
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFUInt[W]): DFUInt[W] = ???
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG, check: Arg.LargerThan1.CheckNUB[V]
  ): DFUInt[IntP.CLog2[V]] = ???
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFCG, check: Arg.Positive.CheckNUB[V]
  ): DFUInt[IntP.CLog2[IntP.+[V, 1]]] = ???

  protected object Unsigned
      extends Check1[
        Boolean,
        [S <: Boolean] =>> ![S],
        [S <: Boolean] =>> "Argument must be unsigned"
      ]
  protected object `UB > R`
      extends Check2[
        Int,
        Int,
        [UB <: Int, R <: Int] =>> UB > R,
        [UB <: Int, R <: Int] =>> "The argument must be smaller than the upper-bound " + UB +
          " but found: " + R
      ]
  protected object `UBW == RW`
      extends Check2[
        Int,
        Int,
        [UBW <: Int, RW <: Int] =>> UBW == RW,
        [UBW <: Int, RW <: Int] =>> "Expected argument width " + UBW + " but found: " + RW +
          "\nTo Fix:\nUse `.resize` to match the width automatically."
      ]

  object Val:
    trait UBArg[UB <: IntP, R] extends Exact1.TC[IntP, UB, [ub <: IntP] =>> IntParam[ub], R, DFC]:
      type OutP
      type Out = DFValTP[DFInt32, OutP]
      def conv(arg1: IntParam[UB], from: R)(using DFC): Out = apply(arg1, from)
      def apply(ub: IntParam[UB], arg: R)(using DFC): Out
    trait UBArgLP:
      transparent inline given errorDMZ[UB <: Int, R](using
          r: ShowType[R]
      ): UBArg[UB, R] =
        Error.call[
          (
              "Upper-bound argument cannot be constructed from the type `",
              r.Out,
              "`."
          )
        ]
    object UBArg extends UBArgLP:
      type Aux[UB <: IntP, R, P] = UBArg[UB, R] { type OutP = P }
      type Exact[UB <: IntP] = Exact1[IntP, UB, [ub <: IntP] =>> IntParam[ub], DFC, UBArg]
      given fromInt[UB <: Int, R <: Int](using
          unsignedCheck: Unsigned.Check[R < 0],
          ubCheck: `UB > R`.CheckNUB[UB, R]
      ): UBArg[UB, R] with
        type OutP = CONST
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out = ???
      given fromR[
          UB <: IntP, R, S <: Boolean, W <: IntP, N <: NativeType, P
      ](using
          ic: DFXInt.Val.Candidate.Aux[R, S, W, N, P]
      )(using
          unsignedCheck: Unsigned.Check[S],
          widthCheck: `UBW == RW`.CheckNUB[IntP.CLog2[UB], W]
      ): UBArg[UB, R] with
        type OutP = P
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out = ???
    end UBArg
    object Ops:
      extension [W <: IntP, P](lhs: DFValTP[DFUInt[W], P])
        def signed(using DFCG): DFValTP[DFSInt[IntP.+[W, 1]], P] = ???
        @scala.annotation.targetName("negateDFUInt")
        def unary_-(using DFCG): DFValTP[DFSInt[IntP.+[W, 1]], P] = ???
        @scala.annotation.targetName("toIntDFUInt")
        def toInt(using dfc: DFCG, check: `W <= 31`.CheckNUB[W]): DFValTP[DFInt32, P] = ???
      end extension
    end Ops
  end Val

end DFUInt

type DFSInt[W <: IntP] = DFXInt[true, W, BitAccurate]
object DFSInt:
  def apply[W <: IntP](width: IntParam[W])(using DFCG, Width.CheckNUB[true, W]): DFSInt[W] = ???
  def forced[W <: IntP](width: IntP)(using DFC): DFSInt[W] = ???
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFSInt[W]): DFSInt[W] = ???
  def untilAbs[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG, check: Arg.LargerThan1.CheckNUB[V]
  ): DFSInt[IntP.+[IntP.CLog2[V], 1]] = ???
  def toAbs[V <: IntP](max: IntParam[V])(using
      dfc: DFCG, check: Arg.Positive.CheckNUB[V]
  ): DFSInt[IntP.+[IntP.CLog2[IntP.+[V, 1]], 1]] = ???

  object Val:
    object Ops:
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @scala.annotation.targetName("negateDFSInt")
        def unary_-(using DFCG): DFValTP[DFSInt[W], P] = ???
        def signbit(using dfc: DFCG): DFValTP[DFBit, P] = ???
        def unsigned(using DFCG): DFValTP[DFUInt[IntP.-[W, 1]], P] = ???
      extension [P](lhs: DFValTP[DFInt32, P])
        @scala.annotation.targetName("negateDFInt32")
        def unary_-(using DFCG): DFValTP[DFInt32, P] = ???
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @scala.annotation.targetName("toIntDFSInt")
        def toInt(using dfc: DFCG, check: `W <= 32`.CheckNUB[W]): DFValTP[DFInt32, P] = ???
    end Ops
  end Val
end DFSInt

//a native Int32 decimal has no explicit Scala compile-time width, since the
//actual value determines its width.
type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]] // This means: DFDecimal[Boolean, Int, 0, Int32] (could not be defined this way because of type recursion)
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
object DFConstInt32:
  def apply(int: Int, named: Boolean = false)(using DFC): DFConstInt32 =
    DFVal.Const(DFInt32, Some(BigInt(int)), named)
