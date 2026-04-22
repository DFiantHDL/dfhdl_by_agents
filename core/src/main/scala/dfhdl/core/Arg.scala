package dfhdl.core
import dfhdl.internals.*

object Arg:
  object Width extends Check1[Int, [t <: Int] =>> true, [t <: Int] =>> ""]
end Arg
