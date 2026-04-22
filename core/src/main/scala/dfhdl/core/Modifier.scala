package dfhdl.core

sealed class Modifier[+A, +C, +I, +P]
type ModifierAny = Modifier[Any, Any, Any, Any]
object Modifier:
  type CONST = Modifier[Any, Any, Any, dfhdl.core.CONST]
end Modifier

sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]
