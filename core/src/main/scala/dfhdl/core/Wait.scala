package dfhdl.core

opaque type Wait <: Unit = Unit
object Wait:
  def apply(trigger: Any)(using DFC): Unit = ???
  trait ContainerOps
  object Ops
end Wait
