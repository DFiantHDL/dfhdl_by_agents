package dfhdl
protected object hdl:
  class dsn extends scala.annotation.StaticAnnotation
  import core.IntP
  export core.DFBoolOrBit.Val.Ops.*
  export core.DFDecimal.StrInterpOps.{d, sd}
  export core.DFDecimal.Val.Ops.*
  export core.DFEnum.Val.Ops.*
  export core.DFVector.Val.Ops.*
  export core.DFOpaque.Val.Ops.*
  export core.DFTuple.Val.Ops.*
  export core.DFVector.Ops.*
  export core.TDFDouble.Val.Ops.*
  export core.TDFString.Val.Ops.*
  export core.DFVal.Ops.*
  export core.DFVarOps.*
  export core.ConnectOps.*
  export core.Conditional.Ops.*
  export core.TextOut.Ops.*
  export compiler.ir.TextOut.Severity
  export internals.CommonOps.*
  export core.{dfType}
  export core.DFPhysical.Val.Ops.*
  export core.LoopOps.*
end hdl

export hdl.*
