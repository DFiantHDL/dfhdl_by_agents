package dfhdl.core
import ir.DFVal.Func.Op as FuncOp

type BitNum = 0 | 1
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  given DFBool = DFBool

  object Val:
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      type OutP
    object Candidate:
      type Types = DFValOf[DFBoolOrBit] | Boolean | BitNum
      type Aux[R, T <: DFBoolOrBit, P] = Candidate[R] { type OutT = T; type OutP = P }
    end Candidate

    object Ops:
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

    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
