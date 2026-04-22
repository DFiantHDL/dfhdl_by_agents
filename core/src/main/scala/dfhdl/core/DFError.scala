package dfhdl.core

sealed abstract class DFError(val dfMsg: String) extends Exception(dfMsg)

object DFError:
  class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  )(using dfc: DFC) extends DFError(iae.getMessage)
  final class REG_DIN[T <: DFTypeAny](val dfVar: DFVarOf[T])(using dfc: DFC)
      extends Basic("Read access", new IllegalArgumentException("")):
    var firstTime: Boolean = true
end DFError

