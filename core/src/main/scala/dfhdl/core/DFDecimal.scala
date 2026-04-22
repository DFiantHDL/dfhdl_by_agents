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
  given DFInt32 = DFInt32
  object Extensions

  protected[core] object Constraints:
    object Width extends Check2[Boolean, Int, [s <: Boolean, w <: Int] =>> true, [s <: Boolean, w <: Int] =>> ""]
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
    object TC
    object TCConv
    object Compare
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
  object Val:
    object Ops
  end Val
end DFUInt

type DFSInt[W <: IntP] = DFXInt[true, W, BitAccurate]
object DFSInt:
  object Val:
    object Ops
  end Val
end DFSInt

//a native Int32 decimal has no explicit Scala compile-time width, since the
//actual value determines its width.
type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]] // This means: DFDecimal[Boolean, Int, 0, Int32] (could not be defined this way because of type recursion)
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
object DFConstInt32:
  def apply(int: Int, named: Boolean = false)(using DFC): DFConstInt32 = ???
