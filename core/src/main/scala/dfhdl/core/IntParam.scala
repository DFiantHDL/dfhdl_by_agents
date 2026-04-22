package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import compiletime.ops.int
import int.*
import compiletime.{constValueOpt, constValue}
import dfhdl.internals.Inlined
import scala.annotation.targetName

type IntP = Int | DFConstInt32 | IntP.Sig
object IntP:
  sealed trait Sig:
    val value: DFConstInt32
  object Sig:
    given [S <: Sig](using s: S): ValueOf[S] = ValueOf[S](s)
    given [F <: FuncOp, L <: IntP, R <: IntP](using
        ValueOf[F],
        ValueOf[L],
        ValueOf[R],
        DFC
    ): Sig2[F, L, R] with
      val value: DFConstInt32 = ???
  sealed trait Sig1[F <: FuncOp, A <: IntP] extends Sig
  sealed trait Sig2[F <: FuncOp, A <: IntP, B <: IntP] extends Sig
  type +[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.+[L, R]
    case _          => Sig2[FuncOp.+.type, L, R]
  type -[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.-[L, R]
    case _          => Sig2[FuncOp.-.type, L, R]
  type *[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.*[L, R]
    case _          => Sig2[FuncOp.*.type, L, R]
  type /[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int./[L, R]
    case _          => Sig2[FuncOp./.type, L, R]
  type %[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.%[L, R]
    case _          => Sig2[FuncOp.%.type, L, R]
  infix type Max[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.Max[L, R]
    case _          => Sig2[FuncOp.max.type, L, R]
  infix type Min[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.Min[L, R]
    case _          => Sig2[FuncOp.min.type, L, R]
  type CLog2[T <: IntP] <: IntP = T match
    case Int => 32 - NumberOfLeadingZeros[T - 1]
    case _   => Sig1[FuncOp.clog2.type, T]
  type Abs[T <: IntP] <: IntP = T match
    case Int => int.Abs[T]
    case _   => Sig1[FuncOp.abs.type, T]
end IntP

into opaque type IntParam[V <: IntP] = Int | DFConstInt32
object IntParam:
  inline implicit def fromValue[T <: IntP & Singleton](inline value: T): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
end IntParam
