package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName, nowarn}
import scala.quoted.*
import scala.util.boundary, boundary.break

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  def apply[W <: IntP](width: IntParam[W])(using
      dfc: DFCG, check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = ???
  def forced[W <: IntP](width: Int): DFBits[W] = ???
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFBits[W]): DFBits[W] = ???
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG, check: Arg.LargerThan1.CheckNUB[V]
  ): DFBits[IntP.CLog2[V]] = ???
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFCG, check: Arg.Positive.CheckNUB[V]
  ): DFBits[IntP.CLog2[IntP.+[V, 1]]] = ???

  given [W <: IntP & Singleton](using
      dfc: DFCG, v: ValueOf[W], check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = ???

  protected object `AW == TW` extends Check2[Int, Int, [AW <: Int, TW <: Int] =>> true, [AW <: Int, TW <: Int] =>> ""]
  protected object `LW >= RW` extends Check2[Int, Int, [LW <: Int, RW <: Int] =>> true, [LW <: Int, RW <: Int] =>> ""]
  protected[core] object BitIndex extends Check2[Int, Int, [I <: Int, W <: Int] =>> true, [I <: Int, W <: Int] =>> ""]
  protected[core] object BitsHiLo extends Check2[Int, Int, [H <: Int, L <: Int] =>> true, [H <: Int, L <: Int] =>> ""]
  trait CompareCheck[ValW <: IntP, ArgW <: IntP, Castle <: Boolean]:
    def apply(dfValWidth: Int, argWidth: Int): Unit
  object CompareCheck:
    given [ValW <: IntP, ArgW <: IntP, Castle <: Boolean]: CompareCheck[ValW, ArgW, Castle] with
      def apply(dfValWidth: Int, argWidth: Int): Unit = ???



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
        def apply(value: R)(using DFC): Out = ???
      given fromDFBoolOrBit[P, R <: DFValTP[DFBoolOrBit, P]]: Candidate[R] with
        type OutW = 1
        type OutP = P
        def apply(value: R)(using DFC): Out = ???
      given fromDFUInt[W <: IntP, P, R <: DFValTP[DFUInt[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out = ???
      transparent inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        ).asInstanceOf[Dud[E]]
      transparent inline given errDFSInt[W <: IntP, R <: DFValOf[DFSInt[W]]]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        ).asInstanceOf[Dud[R]]

      private[Val] def valueToBits(value: Any)(using dfc: DFC): DFValOf[DFBits[Int]] = ???
      transparent inline given fromTuple[R <: NonEmptyTuple]: Candidate[R] = ${ DFBitsMacro[R] }
      object TupleCandidate extends Candidate[Any]:
        def apply(value: Any)(using DFC): Out = ???

      def DFBitsMacro[R](using Quotes, Type[R]): Expr[Candidate[R]] = ???
    end Candidate

    object TC:
      import DFVal.TC
      def apply(dfType: DFBits[Int], dfVal: DFValOf[DFBits[Int]])(using DFC): DFValOf[DFBits[Int]] = ???
      protected object `LW == RW`
          extends Check2[Int, Int, [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "width mismatch"]
      given DFBitsFromCandidate[LW <: IntP, V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      )(using check: `LW == RW`.CheckNUB[LW, ic.OutW]): TC[DFBits[LW], V] with
        type OutP = RP
        def conv(dfType: DFBits[LW], value: V)(using dfc: DFC): Out = ???
    end TC

    object TCConv:
      import DFVal.TCConv
      given DFBitsFromCandidateConv[V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      ): TCConv[DFBits[Int], V] with
        type OutP = RP
        def apply(value: V)(using DFC): Out = ???

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[
          LW <: IntP, R, RP, IC <: Candidate[R],
          Op <: FuncOp.===.type | FuncOp.=!=.type, C <: Boolean
      ](using ic: IC { type OutP = RP })(using
          check: CompareCheck[LW, ic.OutW, C]
      ): Compare[DFBits[LW], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFBits[LW], arg: R)(using DFC): Out = ???
    end Compare

    object TupleOps:
      extension (inline tpl: NonEmptyTuple)
        transparent inline def toBits(using dfc: DFCG): Any = ${ bitsMacro('tpl)('dfc) }
      private def bitsMacro(tpl: Expr[NonEmptyTuple])(dfc: Expr[DFCG])(using Quotes): Expr[Any] = ???
    end TupleOps

    object Ops
  end Val
end DFBits
