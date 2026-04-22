package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+TFE <: DFOpaque.Abstract] = DFType[ir.DFOpaque, Args1[TFE @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, Product, Serializable:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType
  object Abstract:
    type Aux[TFE <: Abstract, AT <: DFTypeAny] = TFE { type ActualType = AT }
  abstract class Frontend[A <: DFTypeAny](final protected[core] val actualType: A) extends Abstract:
    type ActualType = A
  abstract class Magnet[A <: DFTypeAny](actualType: A) extends Frontend[A](actualType)
  abstract class Clk extends Magnet[DFBit](DFBit)
  abstract class Rst extends Magnet[DFBit](DFBit)
  given [TFE <: Abstract](using ce: ClassEv[TFE], dfc: DFCG): DFOpaque[TFE] = ???
  def apply[TFE <: Abstract](t: TFE)(using dfc: DFCG): DFOpaque[TFE] = ???
  extension [A <: DFTypeAny, TFE <: Frontend[A]](dfType: DFOpaque[TFE])
    def actualType: A = ???
    def opaqueType: TFE = ???
  object Val:
    object TC
    object Ops:
      given evOpAsDFOpaqueComp[L, Comp <: Object, TFE <: Abstract](using
          cc: CaseClass.Aux[Comp, Abstract, TFE]
      )(using ce: ClassEv[TFE])(using
          tc: DFVal.TC[ce.value.ActualType, L]
      ): ExactOp2Aux["as", DFC, DFValAny, L, Comp, DFValTP[DFOpaque[TFE], tc.OutP]] = ???
      given evOpAsDFOpaqueTFE[L, T <: DFTypeAny, TFE <: Frontend[T]](using
          tc: DFVal.TC[T, L]
      ): ExactOp2Aux["as", DFC, DFValAny, L, TFE, DFValTP[DFOpaque[TFE], tc.OutP]] = ???
      given evOpAsDFOpaqueIterable[A <: DFTypeAny, P, L <: Iterable[DFValTP[A, P]], T <: DFTypeAny, TFE <: Frontend[T]](using
          tc: DFVal.TC[T, DFValTP[DFVector[A, Tuple1[Int]], P]]
      ): ExactOp2Aux["as", DFC, DFValAny, L, TFE, DFValTP[DFOpaque[TFE], tc.OutP]] = ???
      given evOpClkAsClkComp[LTFE <: Clk, L <: DFValOf[DFOpaque[LTFE]], Comp <: Object, TFE <: Clk](using
          cc: CaseClass.Aux[Comp, Clk, TFE]
      )(using ce: ClassEv[TFE]): ExactOp2Aux["as", DFC, DFValAny, L, Comp, DFValOf[DFOpaque[TFE]]] = ???
      given evOpRstAsRstComp[LTFE <: Rst, L <: DFValOf[DFOpaque[LTFE]], Comp <: Object, TFE <: Rst](using
          cc: CaseClass.Aux[Comp, Rst, TFE]
      )(using ce: ClassEv[TFE]): ExactOp2Aux["as", DFC, DFValAny, L, Comp, DFValOf[DFOpaque[TFE]]] = ???
end DFOpaque
