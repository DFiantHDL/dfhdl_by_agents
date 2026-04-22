package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance

type FieldsOrTuple = DFStruct.Fields | NonEmptyTuple | NamedTuple.AnyNamedTuple
type DFStruct[+F <: FieldsOrTuple] = DFType[ir.DFStruct, Args1[F @uncheckedVariance]]
object DFStruct:
  abstract class Fields extends Product with Serializable
  private[core] def apply[F <: FieldsOrTuple](product: F)(using DFC): DFStruct[F] = ???
  private[core] def unapply(product: Product)(using DFC): Option[DFStruct[FieldsOrTuple]] = ???
  inline given apply[F <: FieldsOrTuple](using dfc: DFCG): DFStruct[F] = ???

  type FieldsWithModifier[F <: FieldsOrTuple, M <: ModifierAny] <: FieldsOrTuple = F match
    case AnyRef => F
  object Val:
    private[core] def unapply(fields: Product)(using DFC): Option[DFValOf[DFStruct[FieldsOrTuple]]] = ???
    object TC
    object Compare
end DFStruct
