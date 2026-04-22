package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFTuple[+T <: NonEmptyTuple] = DFStruct[T @uncheckedVariance]
object DFTuple

