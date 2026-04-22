package dfhdl.core
import dfhdl.internals.*
final class SameElementsVector[T](val value: T)
object SameElementsVector:
  def apply[T](exact: Inlined[T]): SameElementsVector[T] = ???
  def unapply[T, R](arg: SameElementsVector[T]): Option[R] = ???
  protected[core] def bitsValOf[W <: Int, T <: BitOrBool](
      width: IntParam[W],
      sev: SameElementsVector[T],
      named: Boolean = false
  )(using DFC): DFConstOf[DFBits[W]] = ???
  given eqBit[W <: Int, T <: BitOrBool]: CanEqual[SameElementsVector[T], DFValOf[DFBits[W]]] = CanEqual.derived
  given eqVec[DFT <: DFTypeAny, D <: NonEmptyTuple, T]
      : CanEqual[SameElementsVector[T], DFValOf[DFVector[DFT, D]]] = CanEqual.derived
end SameElementsVector
