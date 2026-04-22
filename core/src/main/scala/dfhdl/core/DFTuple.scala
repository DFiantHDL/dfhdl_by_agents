package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type DFTuple[+T <: NonEmptyTuple] = DFStruct[T @uncheckedVariance]
object DFTuple:
  private[core] def apply[T <: NonEmptyTuple](t: NonEmptyTuple)(using DFC): DFTuple[T] = ???
  private[core] def apply[T <: NonEmptyTuple](fieldList: List[DFTypeAny])(using DFC): DFTuple[T] = ???
  private[core] def unapply(t: NonEmptyTuple)(using DFC): Option[DFTuple[NonEmptyTuple]] = ???

  trait TCZipper[T <: NonEmptyTuple, V <: NonEmptyTuple, O, TC[T <: DFTypeAny, V] <: TCCommon[T, V, O]]:
    type OutP
    def apply(fieldList: List[DFTypeAny], tupleValues: List[Any]): List[O]
  object TCZipper

  object Val:
    private[core] def unapply(tuple: Tuple)(using DFC): Option[DFValOf[DFTuple[NonEmptyTuple]]] = ???
    object TC
    object TCConv
    object Compare
    object Ops
end DFTuple
