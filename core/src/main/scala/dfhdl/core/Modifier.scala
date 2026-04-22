package dfhdl.core

sealed class Modifier[+A, +C, +I, +P]
type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  sealed trait Assignable
  type Mutable = Modifier[Assignable, Any, Any, dfhdl.core.NOTCONST]
  type Dcl = Modifier[Assignable, Any, Any, dfhdl.core.NOTCONST]
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
end Modifier

sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
type NOTCONST = Any
