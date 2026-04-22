package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]
object DFVector

