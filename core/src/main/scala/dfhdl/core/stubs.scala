package dfhdl.core

// stubs replacing dfhdl.compiler.ir and dfhdl.internals
object ir:
  trait DFType
  trait DFBits extends DFType
  trait DFBoolOrBit extends DFType
  trait DFBool extends DFType
  object DFBool extends DFBool
  trait DFDecimal extends DFType
  trait DFVal
  trait Meta
  val DFInt32: DFType = ???
  object DFVal:
    object Func:
      sealed trait Op
      object Op:
        sealed trait `+` extends Op; object `+` extends `+`
        sealed trait `-` extends Op; object `-` extends `-`
        sealed trait `*` extends Op; object `*` extends `*`
        sealed trait `/` extends Op; object `/` extends `/`
        sealed trait `%` extends Op; object `%` extends `%`
        sealed trait max extends Op; object max extends max
        sealed trait min extends Op; object min extends min
        sealed trait `|` extends Op; object `|` extends `|`
        sealed trait `&` extends Op; object `&` extends `&`
        sealed trait `^` extends Op; object `^` extends `^`
  object DFDecimal:
    sealed trait NativeType
    object NativeType:
      sealed trait Int32 extends NativeType
      sealed trait BitAccurate extends NativeType
end ir

trait Position
trait MetaContext
trait ExactOp2Aux[Op, C, B, L, R, O]
