package dfhdl.core

import dfhdl.compiler.ir

opaque type DomainType <: ir.DomainType = ir.DomainType
object DomainType:
  opaque type DF <: DomainType = DomainType
  opaque type RT <: DomainType = DomainType
  opaque type ED <: DomainType = DomainType
