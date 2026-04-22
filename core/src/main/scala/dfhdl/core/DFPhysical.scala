package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName

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
        def apply(value: DFValTP[DFPhysical[U], P])(using DFC): DFValTP[DFPhysical[U], P] = ???
    given fromDFIntOrDFDouble[P]: TC[DFValTP[DFInt32 | DFDouble, P]] with
      type OutU = ir.LiteralNumber
      type OutP = P
      def apply(value: DFValTP[DFInt32 | DFDouble, P])(using DFC): DFValTP[DFNumber, OutP] = ???
    given TC[Int] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Int)(using DFC): DFValTP[DFNumber, OutP] = ???
    given TC[Long] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Long)(using DFC): DFValTP[DFNumber, OutP] = ???
    given TC[Double] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Double)(using DFC): DFValTP[DFNumber, OutP] = ???
  end TC
  object Val:
    object Ops:
      trait OpTC[Op <: FuncOp, LU <: ir.PhysicalNumber, RU <: ir.PhysicalNumber]:
        type OutU <: ir.PhysicalNumber
        def apply(lhsDFType: DFPhysical[LU], rhsDFType: DFPhysical[RU]): DFPhysical[OutU]
      object OpTC:
        type Aux[Op <: FuncOp, LU <: ir.PhysicalNumber, RU <: ir.PhysicalNumber, OU <: ir.PhysicalNumber] =
          OpTC[Op, LU, RU] { type OutU = OU }
      given evOpArithDFPhysical[
          Op <: FuncOp.+.type | FuncOp.-.type | FuncOp.*.type | FuncOp./.type,
          U <: ir.PhysicalNumber, LP, L <: DFValTP[DFPhysical[U], LP],
          R, RU <: ir.PhysicalNumber, RP, OU <: ir.PhysicalNumber
      ](using
          tcR: TC.Aux[R, RU, RP],
          tcOp: OpTC.Aux[Op, U, RU, OU]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFPhysical[OU], LP | RP]] = ???
    end Ops
  end Val
end DFPhysical

type DFTime = DFPhysical[ir.TimeNumber]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[ir.FreqNumber]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[ir.LiteralNumber]
val DFNumber = ir.DFNumber.asFE[DFNumber]
