package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName, nowarn}
import scala.quoted.*
import scala.util.boundary, boundary.break
import DFDecimal.Constraints.`LW == RW`

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  def apply[W <: IntP](width: IntParam[W])(using
      dfc: DFCG,
      check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = trydf:
    width.toScalaIntOpt.foreach(check(_))
    ir.DFBits(width.ref).asFE[DFBits[W]]
  def forced[W <: IntP](width: Int): DFBits[W] =
    val check = summon[Arg.Width.Check[Int]]
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFBits[W]): DFBits[W] = trydf { dfType }
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFBits[IntP.CLog2[V]] = trydf:
    sup.toScalaIntOpt.foreach(check(_))
    ir.DFBits(sup.clog2.ref).asFE[DFBits[IntP.CLog2[V]]]
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFCG,
      check: Arg.Positive.CheckNUB[V]
  ): DFBits[IntP.CLog2[IntP.+[V, 1]]] = trydf:
    max.toScalaIntOpt.foreach(check(_))
    ir.DFBits((max + 1).clog2.ref).asFE[DFBits[IntP.CLog2[IntP.+[V, 1]]]]

  given [W <: IntP & Singleton](using
      dfc: DFCG,
      v: ValueOf[W],
      check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = trydf:
    val width = IntParam.forced(v)
    width.toScalaIntOpt.foreach(check(_))
    ir.DFBits(width.ref).asFE[DFBits[W]]

  protected object `AW == TW`
      extends Check2[
        Int,
        Int,
        [AW <: Int, TW <: Int] =>> AW == TW,
        [AW <: Int, TW <: Int] =>> "The alias width (" + AW +
          ") is different than the DFHDL value width (" + TW + ")."
      ]
  protected object `LW >= RW`
      extends Check2[
        Int,
        Int,
        [LW <: Int, RW <: Int] =>> LW >= RW,
        [LW <: Int, RW <: Int] =>> "The new width (" + RW +
          ") is larger than the original width (" + LW + ")."
      ]
  protected[core] object BitIndex
      extends Check2[
        Int,
        Int,
        [I <: Int, W <: Int] =>> (I < W) && (I >= 0),
        [I <: Int, W <: Int] =>> "Index " + I + " is out of range of width/length " + W
      ]
  protected[core] object BitsHiLo
      extends Check2[
        Int,
        Int,
        [H <: Int, L <: Int] =>> H >= L,
        [H <: Int, L <: Int] =>> "Low index " + L + " is bigger than High bit index " + H
      ]
  trait CompareCheck[
      ValW <: IntP,
      ArgW <: IntP,
      Castle <: Boolean // castling of dfVal and arg
  ]:
    def apply(dfValWidth: Int, argWidth: Int): Unit
  object CompareCheck:
    given [
        ValW <: IntP,
        ValWI <: Int,
        ArgW <: IntP,
        ArgWI <: Int,
        Castle <: Boolean
    ](using
        ubv: UBound.Aux[Int, ValW, ValWI],
        uba: UBound.Aux[Int, ArgW, ArgWI],
        lw: Id[ITE[Castle, ArgWI, ValWI]],
        rw: Id[ITE[Castle, ValWI, ArgWI]]
    )(using
        checkW: `LW == RW`.Check[lw.Out, rw.Out],
        castle: ValueOf[Castle]
    ): CompareCheck[ValW, ArgW, Castle] with
      def apply(dfValWidth: Int, argWidth: Int): Unit =
        val lw = if (castle) argWidth else dfValWidth
        val rw = if (castle) dfValWidth else argWidth
        checkW(lw, rw)
    end given
  end CompareCheck



  object StrInterpOps:
    extension (sc: StringContext)
      def h(args: Any*)(using DFC): Any = ???
      def b(args: Any*)(using DFC): Any = ???

  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutW <: IntP
      type OutP
      type Out = DFValTP[DFBits[OutW], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(value: R)(using DFC): Out
    trait CandidateLP:
      given fromIf[
          C <: DFValOf[DFBoolOrBit],
          T,
          F,
          TW <: IntP,
          TP,
          FP,
          R <: IfWrapper[C, T, F]
      ](using
          tTC: Candidate[T] { type OutW = TW; type OutP = TP },
          fTC: DFVal.TC[DFBits[TW], F] { type OutP = FP }
      ): Candidate[R] with
        type OutW = TW
        type OutP = TP | FP
        def apply(value: R)(using DFC): Out = value.unwrap
      end fromIf
    end CandidateLP
    object Candidate extends CandidateLP:
      type Exact = Exact0[DFC, Candidate]
      type Aux[R, W <: IntP, P] = Candidate[R] { type OutW = W; type OutP = P }
      type Dud[V] = Candidate[V]:
        type OutW = Int
        type OutP = NOTCONST
      transparent inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(
          "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal constant via the `d\"<width>'<number>\"` string interpolation."
        ).asInstanceOf[Dud[V]]
      given fromDFBits[W <: IntP, P, R <: DFValTP[DFBits[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out = value
      given fromDFBoolOrBit[P, R <: DFValTP[DFBoolOrBit, P]]: Candidate[R] with
        type OutW = 1
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          value.bits
      given fromDFUInt[W <: IntP, P, R <: DFValTP[DFUInt[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          if (value.hasTag[ir.ResizeTag]) value.bits.tag(ir.ResizeTag)
          else value.bits
      transparent inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        ).asInstanceOf[Dud[E]]
      transparent inline given errDFSInt[W <: IntP, R <: DFValOf[DFSInt[W]]]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        ).asInstanceOf[Dud[R]]

      private[Val] def valueToBits(value: Any)(using dfc: DFC): DFValOf[DFBits[Int]] =
        import DFBits.Val.Ops.concatBits
        val dfcAnon = dfc.anonymize
        value match
          case x: NonEmptyTuple =>
            x.toList.map(x => valueToBits(x)(using dfcAnon)).concatBits
          case i: Int =>
            DFVal.Const(DFBits(1), (BitVector.bit(i > 0), BitVector.zero), named = true)
          case dfVal: DFVal[?, ?] =>
            import DFVal.Ops.bits
            val dfValIR = dfVal.asIR
            dfValIR.dfType match
              case _: ir.DFBits => dfValIR.asValOf[DFBits[Int]]
              case _            =>
                dfValIR.asValAny.bits(using dfc)(using Width.wide).asValOf[DFBits[Int]]
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple]: Candidate[R] = ${ DFBitsMacro[R] }
      object TupleCandidate extends Candidate[Any]:
        def apply(value: Any)(using DFC): Out =
          valueToBits(value).asInstanceOf[Out]

      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        '{
          TupleCandidate.asInstanceOf[
            Candidate[R] {
              type OutW = wType.Underlying
              type OutP = pType.Underlying
            }
          ]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFBits[Int],
          dfVal: DFValOf[DFBits[Int]]
      )(using DFC): DFValOf[DFBits[Int]] =
        (dfType.widthIntOpt, dfVal.widthIntOpt) match
          case (Some(lw), Some(rw)) => `LW == RW`(lw, rw)
          case _                    =>
        dfVal
      protected object `LW == RW`
          extends Check2[
            Int,
            Int,
            [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "The argument width (" + ToString[RW] +
              ") is different than the receiver width (" + ToString[LW] +
              ").\nConsider applying `.resize` to resolve this issue."
          ]
      given DFBitsFromCandidate[LW <: IntP, V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      )(using
          check: `LW == RW`.CheckNUB[LW, ic.OutW]
      ): TC[DFBits[LW], V] with
        type OutP = RP
        def conv(dfType: DFBits[LW], value: V)(using dfc: DFC): Out =
          import Ops.resizeBits
          val dfVal = ic(value)
          if (dfVal.hasTag[ir.ResizeTag])
            dfVal.resizeBits(dfType.widthIntParam).asValTP[DFBits[LW], RP]
          else
            (dfType.widthIntOpt, dfVal.widthIntOpt) match
              case (Some(lw), Some(rw)) => check(lw, rw)
              case _                    =>
                if (dfType.compareWidths(dfVal.dfType)(_ != _).getOrElse(true))
                  throw new IllegalArgumentException(
                    s"""|The argument width (${dfVal.dfType.widthCodeString}) is different than the receiver width (${dfType.widthCodeString}).
                        |Consider applying `.resize` to resolve this issue.""".stripMargin
                  )
            dfVal.nameInDFCPosition.asValTP[DFBits[LW], RP]
          end if
        end conv
      end DFBitsFromCandidate
      given DFBitsFromSEV[LW <: IntP, T <: BitOrBool, V <: SameElementsVector[T]]: TC[DFBits[LW], V]
      with
        type OutP = CONST
        def conv(dfType: DFBits[LW], value: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, value, named = true)
            .asConstOf[DFBits[LW]]
    end TC

    object TCConv:
      import DFVal.TCConv
      given DFBitsFromCandidateConv[V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      ): TCConv[DFBits[Int], V] with
        type OutP = RP
        def apply(value: V)(using DFC): Out =
          val dfVal = ic(value)
          dfVal.nameInDFCPosition.asValTP[DFBits[Int], RP]

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[
          LW <: IntP,
          R,
          RP,
          IC <: Candidate[R],
          Op <: FuncOp.===.type | FuncOp.=!=.type,
          C <: Boolean
      ](
          using ic: IC { type OutP = RP }
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFBits[LW], arg: R)(using DFC): Out =
          val dfValArg = ic(arg)
          (dfType.widthIntOpt, dfValArg.dfType.widthIntOpt) match
            case (Some(lw), Some(rw)) => check(lw, rw)
            case _                    =>
              if (dfType.compareWidths(dfValArg.dfType)(_ != _).getOrElse(true))
                val lhsStr =
                  if (castling) dfValArg.dfType.widthCodeString else dfType.widthCodeString
                val rhsStr =
                  if (castling) dfType.widthCodeString else dfValArg.dfType.widthCodeString
                throw new IllegalArgumentException(
                  s"""|Cannot apply this operation between a value of $lhsStr bits width (LHS) and a value of $rhsStr bits width (RHS).
                      |An explicit conversion must be applied.""".stripMargin
                )
          dfValArg.asValTP[DFBits[LW], RP]
        end conv
      end DFBitsCompareCandidate
      given DFBitsCompareSEV[
          LW <: IntP,
          Op <: FuncOp.===.type | FuncOp.=!=.type,
          C <: Boolean,
          T <: BitOrBool,
          V <: SameElementsVector[T]
      ](using
          ValueOf[Op],
          ValueOf[C]
      ): Compare[DFBits[LW], V, Op, C] with
        type OutP = CONST
        def conv(dfType: DFBits[LW], arg: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, arg, named = true)
            .asConstOf[DFBits[LW]]
      end DFBitsCompareSEV
    end Compare

    // this was defined separately from `Ops` to avoid collision with `.bits` used in `Ops`
    object TupleOps:
      // explicit conversion of a tuple to bits (concatenation)
      extension (inline tpl: NonEmptyTuple)
        transparent inline def toBits(using dfc: DFCG): Any = ${ bitsMacro('tpl)('dfc) }
      private def bitsMacro(tpl: Expr[NonEmptyTuple])(dfc: Expr[DFCG])(using Quotes): Expr[Any] =
        import quotes.reflect.*
        val exactInfo = tpl.exactInfo
        import Width.*
        val rTpe = exactInfo.exactTpe
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        '{
          Val.Candidate
            .valueToBits($tpl)(using $dfc)
            .asValTP[DFBits[wType.Underlying], pType.Underlying]
        }
    end TupleOps

    object Ops:
      import IntP.{-, +}
      given evOpApplyDFBits[
          W <: IntP, A, C, I, P,
          L <: DFVal[DFBits[W], Modifier[A, C, I, P]], R
      ](using
          ub: DFUInt.Val.UBArg[W, R]
      ): ExactOp2Aux["apply", DFC, DFValAny, L, R, DFVal[DFBit, Modifier[A, Any, Any, P]]] = ???
      given evOpApplyRangeDFBits[
          W <: IntP, A, C, I, P,
          L <: DFVal[DFBits[W], Modifier[A, C, I, P]],
          HI <: IntP, LO <: IntP
      ](using
          checkHigh: BitIndex.CheckNUB[HI, W],
          checkLow: BitIndex.CheckNUB[LO, W],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, HI, LO, DFVal[DFBits[HI - LO + 1], Modifier[A, Any, Any, P]]] = ???
      given evLogicOpDFBits[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          L, LW <: IntP, LP, R, RW <: IntP, RP
      ](using
          icL: Candidate.Aux[L, LW, LP],
          icR: Candidate.Aux[R, RW, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFBits[LW], LP | RP]] = ???
      given evOpLogicReduceDFBits[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          LW <: IntP, LP,
          L <: DFValTP[DFBits[LW], LP] | DFValTP[DFUInt[LW], LP]
      ](using op: ValueOf[Op]): ExactOp1Aux[Op, DFC, DFValAny, L, DFValTP[DFBit, LP]] = ???
      given evConcatOpDFBits[
          Op <: FuncOp.++.type, L, LW <: IntP, LP, R, RW <: IntP, RP
      ](using
          icL: Candidate.Aux[L, LW, LP],
          icR: Candidate.Aux[R, RW, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFBits[IntP.+[LW, RW]], LP | RP]] = ???
      given evOpShift[
          Op <: FuncOp.>>.type | FuncOp.<<.type,
          LW <: IntP, LP,
          LT <: DFBits[LW] | DFSInt[LW] | DFUInt[LW] | DFInt32,
          L <: DFValTP[LT, LP], R, RP
      ](using
          ub: DFUInt.Val.UBArg.Aux[LW, R, RP]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[LT, LP | RP]] = ???

      extension [W <: IntP, P](lhs: DFValTP[DFBits[W], P])
        private[DFBits] def resizeBits[RW <: IntP](updatedWidth: IntParam[RW])(using DFC): DFValTP[DFBits[RW], P] = ???
        def resize(using DFCG): DFValTP[DFBits[Int], P] = ???
        def resize[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: Arg.Width.CheckNUB[RW], dfc: DFCG
        ): DFValTP[DFBits[RW], P] = ???
      end extension
      extension [T <: Int, P](iter: Iterable[DFValTP[DFBits[T], P]])
        protected[core] def concatBits(using DFC): DFValTP[DFBits[Int], P] = ???
      end extension
      extension [L <: DFValAny, LW <: IntP, LP](lhs: L)(using icL: Candidate.Aux[L, LW, LP])
        def resize(using DFCG): DFValTP[DFBits[Int], icL.OutP] = ???
        def repeat[N <: IntP](num: IntParam[N])(using
            dfc: DFCG, check: Arg.Positive.CheckNUB[N]
        ): DFValTP[DFBits[IntP.*[icL.OutW, N]], icL.OutP | CONST] = ???
      end extension

      given evOpAsDFBits[
          W <: IntP, A, C, I, P,
          L <: DFVal[DFBits[W], Modifier[A, C, I, P]],
          AT <: DFType.Supported, OT <: DFTypeAny, OW <: IntP
      ](using
          tc: DFType.TC.Aux[AT, OT],
          aW: Width.Aux[OT, OW]
      )(using
          check: `AW == TW`.CheckNUB[OW, W]
      ): ExactOp2Aux["as", DFC, DFValAny, L, AT, DFValTP[OT, P]] = ???

      extension [W <: IntP, A, C, I, P](
          lhs: DFVal[DFBits[W], Modifier[A, C, I, P]]
      )
        def uint(using DFCG): DFValTP[DFUInt[W], P] = ???
        def sint(using DFCG): DFValTP[DFSInt[W], P] = ???
        def unary_~(using DFCG): DFValTP[DFBits[W], P] = ???
        def msbit(using DFCG): DFVal[DFBit, Modifier[A, Any, Any, P]] = ???
        def lsbit(using DFCG): DFVal[DFBit, Modifier[A, Any, Any, P]] = ???
        def msbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW], dfc: DFCG
        ): DFValTP[DFBits[RW], P] = ???
        def lsbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW], dfc: DFCG
        ): DFValTP[DFBits[RW], P] = ???
      end extension
    end Ops
  end Val
end DFBits
