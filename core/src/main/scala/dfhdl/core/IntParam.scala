package dfhdl.core
type IntP = Any
object IntP:
  type +[L <: IntP, R <: IntP] = Int
  type Max[L <: IntP, R <: IntP] = Int
end IntP
