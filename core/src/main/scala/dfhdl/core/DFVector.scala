package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]

object DFVector:
  protected[core] def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: List[IntParam[Int]]
  )(using DFC): DFVector[T, D] = ???
  given [T <: DFTypeAny, D <: IntP](using
      dfc: DFCG,
      cellType: T,
      d: ValueOf[D],
      check: VectorLength.CheckNUB[D]
  ): DFVector[T, Tuple1[D]] = ???

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = ???
  extension [T <: DFTypeAny, D1 <: IntP](dfType: DFVector[T, Tuple1[D1]])
    def lengthIntOpt(using dfc: DFC): Option[Int] = ???
    def lengthIntParam(using dfc: DFC): IntParam[D1] = ???

  protected[core] object VectorLength
      extends Check1[
        Int,
        [L <: Int] =>> L > 0,
        [L <: Int] =>> "The vector length must be positive but found: " + L
      ]

  object Ops
  object Val:
    object TC
    object TCConv
    object Compare
    object Ops
end DFVector
