package dfhdl.core
type IntP = Int | DFConstInt32
object IntP:
  type +[L <: IntP, R <: IntP] = Int
  type Max[L <: IntP, R <: IntP] = Int
end IntP
