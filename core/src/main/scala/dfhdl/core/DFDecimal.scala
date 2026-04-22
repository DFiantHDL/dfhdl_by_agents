package dfhdl.core
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.*

import scala.quoted.*
import scala.compiletime.ops.boolean.||

type DFDecimal[S <: Boolean, W <: IntP, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  object StrInterpOps:
    extension (inline sc: StringContext)
      transparent inline def apply(inline args: Any*)(using dfc: DFCG): Any =
        ${ applyMacro('sc, 'args)('dfc) }
    private def applyMacro(
        sc: Expr[StringContext],
        args: Expr[Seq[Any]]
    )(dfc: Expr[DFC])(using Quotes): Expr[Any] = ???
  end StrInterpOps

  object Val:
    object Ops:
      export DFXInt.Val.Ops.*
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: IntP, N <: NativeType] = DFDecimal[S, W, 0, N]
object DFXInt:

  object Val:
    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: IntP
      type OutN <: NativeType
      type OutP
    object Candidate:
      type Aux[R, S <: Boolean, W <: IntP, N <: NativeType, P] =
        Candidate[R] {
          type OutS = S
          type OutW = W
          type OutN = N
          type OutP = P
        }

    object Ops:
      type CommutativeArithOp =
        FuncOp.+.type | FuncOp.*.type | FuncOp.max.type | FuncOp.min.type
      type NonCommutativeArithOp =
        FuncOp.-.type | FuncOp./.type | FuncOp.%.type
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

    end Ops
  end Val
end DFXInt

type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]]
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
