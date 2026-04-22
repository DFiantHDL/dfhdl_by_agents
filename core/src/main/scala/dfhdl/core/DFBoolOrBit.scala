package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}

type BitNum = 0 | 1
type BitOrBool = BitNum | Boolean
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  given DFBool = DFBool
  given DFBit = DFBit

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit DFHDL value."
    )
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutT <: DFBoolOrBit
      type OutP
      type Out = DFValTP[OutT, OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out
    object Candidate:
      type Types = DFValOf[DFBoolOrBit] | Boolean | BitNum | IfWrapper[?, ?, ?]
      type Aux[R, T <: DFBoolOrBit, P] = Candidate[R] { type OutT = T; type OutP = P }
    end Candidate

    object TC

    object Compare

    object Ops:
      import DFDecimal.Constraints
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          L <: Candidate.Types,
          LT <: DFBoolOrBit,
          LP,
          R <: Candidate.Types,
          RT <: DFBoolOrBit,
          RP
      ](using
          icL: Candidate.Aux[L, LT, LP],
          icR: Candidate.Aux[R, RT, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[LT, LP | RP]] = ???
      given evLogicOpDFBoolOrBit2[
          Op <: FuncOp.|.type | FuncOp.&.type,
          L <: Candidate.Types,
          R <: Candidate.Types,
          O <: DFValAny
      ](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] = ???

      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        transparent inline def sel[OT, OF](inline onTrue: OT, inline onFalse: OF)(using
            dfc: DFCG
        ): Any =
          inline onTrue match
            case onTrueDFVal: DFValTP[tt, tp] =>
              val tc = compiletime.summonInline[DFVal.TC[tt, OF]]
              val dfType = onTrueDFVal.dfType
              tc(dfType, onFalse).asValTP[tt, P | tp]
        end sel
      end extension
    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived

type DFConstBool = DFConstOf[DFBool]
type DFConstBit = DFConstOf[DFBit]
