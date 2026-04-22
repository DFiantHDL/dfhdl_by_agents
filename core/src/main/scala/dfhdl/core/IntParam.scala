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
protected sealed trait IntParamLP:
  given [T <: IntP]: Conversion[IntParam[T], IntParam[Int]] = value =>
    value.asInstanceOf[IntParam[Int]]
object IntParam extends IntParamLP:
  given [L <: IntP, R <: IntP](using CanEqual[L, R]): CanEqual[IntParam[L], IntParam[R]] =
    CanEqual.derived
  given [T <: IntP]: CanEqual[IntParam[T], Int] = CanEqual.derived
  given [T <: IntP]: CanEqual[Int, IntParam[T]] = CanEqual.derived

  inline implicit def fromValue[T <: IntP & Singleton](inline value: T): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueInlined")
  inline implicit def fromValue[T <: Int](inline value: Inlined[T]): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueWide")
  inline implicit def fromValue[Wide <: IntP](inline value: Wide): IntParam[Wide] =
    value.asInstanceOf[IntParam[Wide]]
  inline def apply[T <: IntP](inline value: T): IntParam[T] = value match
    case sig: IntP.Sig => sig.value.asInstanceOf[IntParam[T]]
    case _             => value.asInstanceOf[IntParam[T]]
  inline def forced[V <: IntP](inline value: IntP): IntParam[V] = value.asInstanceOf[IntParam[V]]
  @targetName("applyInlined")
  inline def apply[V <: Int](inline value: Inlined[V]): IntParam[V] =
    value.asInstanceOf[IntParam[V]]
  extension [L <: IntP](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstInt32 = ???
    def toScalaIntOpt: Option[Int] = ???
    def toScalaIntUNSAFE: Int = ???
    def ref: ir.IntParamRef = ???
    def +[R <: IntP](rhs: IntParam[R]): IntParam[IntP.+[L, R]] = ???
    def -[R <: IntP](rhs: IntParam[R]): IntParam[IntP.-[L, R]] = ???
    def *[R <: IntP](rhs: IntParam[R]): IntParam[IntP.*[L, R]] = ???
    def /[R <: IntP](rhs: IntParam[R]): IntParam[IntP./[L, R]] = ???
    def %[R <: IntP](rhs: IntParam[R]): IntParam[IntP.%[L, R]] = ???
    infix def max[R <: IntP](rhs: IntParam[R]): IntParam[IntP.Max[L, R]] = ???
    infix def min[R <: IntP](rhs: IntParam[R]): IntParam[IntP.Min[L, R]] = ???
    def clog2: IntParam[IntP.CLog2[L]] = ???
    def =~[R <: IntP](that: IntParam[R]): Boolean = ???
    protected[dfhdl] def cloneAnonValueAndDepsHere: IntParam[Int] = ???
  end extension
end IntParam

extension (intParamRef: ir.IntParamRef)
  def get(using dfc: DFC): IntParam[Int] = ???
  protected[core] def refCodeString(using dfc: DFC): String = ???
