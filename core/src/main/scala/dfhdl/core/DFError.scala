package dfhdl.core

sealed abstract class DFError(val dfMsg: String) extends Exception(dfMsg)

object DFError:
  class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  )(using dfc: DFC) extends DFError(iae.getMessage)
end DFError

