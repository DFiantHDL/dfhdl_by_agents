package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+TFE <: DFOpaque.Abstract] = DFType[ir.DFOpaque, Args1[TFE @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, Product, Serializable:
    type ActualType <: DFTypeAny
  abstract class Frontend[A <: DFTypeAny] extends Abstract:
    type ActualType = A
  abstract class Magnet[A <: DFTypeAny] extends Frontend[A]
  abstract class Clk extends Magnet[DFBit]
  abstract class Rst extends Magnet[DFBit]
  object Val:
    object TC
    object Ops
end DFOpaque
