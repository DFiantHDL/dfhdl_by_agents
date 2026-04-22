package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName
import scala.annotation.implicitNotFound

extension (bd: BigDecimal.type)
  private def apply(arg: Int | Long | Double): BigDecimal = arg match
    case i: Int    => BigDecimal(i)
    case l: Long   => BigDecimal(l)
    case d: Double => BigDecimal(d)

type DFPhysical[U <: ir.PhysicalNumber] = DFType[ir.DFPhysical[U], NoArgs]
object DFPhysical:
  given DFTime = DFTime
  given DFFreq = DFFreq
  given DFNumber = DFNumber
  trait TC[-T]:
    type OutU <: ir.PhysicalNumber
    type OutP
    def apply(value: T)(using DFC): DFValTP[DFPhysical[OutU], OutP]
  object TC:
    type Aux[T, U <: ir.PhysicalNumber, P] = TC[T] { type OutU = U; type OutP = P }
    given [U <: ir.PhysicalNumber, P]: Aux[DFValTP[DFPhysical[U], P], U, P] =
      new TC[DFValTP[DFPhysical[U], P]]:
        type OutU = U
        type OutP = P
        def apply(value: DFValTP[DFPhysical[U], P])(using DFC): DFValTP[DFPhysical[U], P] = value
    given fromDFIntOrDFDouble[P]: TC[DFValTP[DFInt32 | DFDouble, P]] with
      type OutU = ir.LiteralNumber
      type OutP = P
      def apply(value: DFValTP[DFInt32 | DFDouble, P])(using DFC): DFValTP[DFNumber, OutP] =
        Val.Ops.toNumber(value)
    given TC[Int] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Int)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
    given TC[Long] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Long)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
    given TC[Double] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Double)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
  end TC
  object Val:
    object Ops:
      trait OpTC[Op <: FuncOp, LU <: ir.PhysicalNumber, RU <: ir.PhysicalNumber]:
        type OutU <: ir.PhysicalNumber
        def apply(lhsDFType: DFPhysical[LU], rhsDFType: DFPhysical[RU]): DFPhysical[OutU]
      object OpTC:
        type Aux[Op <: FuncOp, LU <: ir.PhysicalNumber, RU <: ir.PhysicalNumber, OU <: ir.PhysicalNumber] =
          OpTC[Op, LU, RU] { type OutU = OU }
      extension (lhs: Int | Long | Double)
        def fs(using DFCG): DFConstOf[DFTime] = ???
        def ps(using DFCG): DFConstOf[DFTime] = ???
        def ns(using DFCG): DFConstOf[DFTime] = ???
        def us(using DFCG): DFConstOf[DFTime] = ???
        def ms(using DFCG): DFConstOf[DFTime] = ???
        def sec(using DFCG): DFConstOf[DFTime] = ???
        def mn(using DFCG): DFConstOf[DFTime] = ???
        def hr(using DFCG): DFConstOf[DFTime] = ???
        def Hz(using DFCG): DFConstOf[DFFreq] = ???
        def KHz(using DFCG): DFConstOf[DFFreq] = ???
        def MHz(using DFCG): DFConstOf[DFFreq] = ???
        def GHz(using DFCG): DFConstOf[DFFreq] = ???
      end extension

      given evOpArithDFPhysical[
          Op <: FuncOp.+.type | FuncOp.-.type | FuncOp.*.type | FuncOp./.type,
          U <: ir.PhysicalNumber, LP, L <: DFValTP[DFPhysical[U], LP],
          R, RU <: ir.PhysicalNumber, RP, OU <: ir.PhysicalNumber
      ](using
          tcR: TC.Aux[R, RU, RP],
          tcOp: OpTC.Aux[Op, U, RU, OU]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFPhysical[OU], LP | RP]] = ???

      extension [LP](lhs: DFValTP[DFInt32 | DFDouble, LP])
        def toNumber(using DFC): DFValTP[DFNumber, LP] = ???
      extension [LP](lhs: DFValTP[DFNumber, LP])
        @targetName("fromNumberToInt")
        def toInt(using DFC): DFValTP[DFInt32, LP] = ???
        def toDouble(using DFC): DFValTP[DFDouble, LP] = ???
    end Ops
  end Val
end DFPhysical

type DFTime = DFPhysical[ir.TimeNumber]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[ir.FreqNumber]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[ir.LiteralNumber]
val DFNumber = ir.DFNumber.asFE[DFNumber]
