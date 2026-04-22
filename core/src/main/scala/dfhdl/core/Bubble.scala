package dfhdl.core

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour derives CanEqual:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def constValOf[T <: DFTypeAny](dfType: T, named: Boolean)(using dfc: DFC): DFConstOf[T] = ???

final val ? = Bubble
