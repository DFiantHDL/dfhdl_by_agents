package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName, nowarn}
import scala.quoted.*
import scala.util.boundary, boundary.break

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  given [W <: IntP & Singleton](using
      dfc: DFCG, v: ValueOf[W], check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = ???

  trait CompareCheck[ValW <: IntP, ArgW <: IntP, Castle <: Boolean]
  object CompareCheck:
    given [ValW <: IntP, ArgW <: IntP, Castle <: Boolean]: CompareCheck[ValW, ArgW, Castle] with {}

  object StrInterpOps

  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutW <: IntP
      type OutP
      type Out = DFValTP[DFBits[OutW], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(value: R)(using DFC): Out
    trait CandidateLP
    object Candidate extends CandidateLP:
      type Aux[R, W <: IntP, P] = Candidate[R] { type OutW = W; type OutP = P }
    end Candidate

    object TC
    object TCConv
    object Compare
    object Ops
  end Val
end DFBits
