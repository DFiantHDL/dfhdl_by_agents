package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName

sealed trait LogEvent derives CanEqual:
  val dfMsg: String

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg), LogEvent

object DFError:
  class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  )(using dfc: DFC) extends DFError(iae.getMessage)
  object FakeEnum extends DFError("")
  final class Derived(from: DFError) extends DFError(from.dfMsg)
  final class REG_DIN[T <: DFTypeAny](val dfVar: DFVarOf[T])(using dfc: DFC)
      extends Basic("Read access", new IllegalArgumentException("")):
    var firstTime: Boolean = true

  extension (dfErr: DFError)
    inline def asNet: DFNet = new DFNet(dfErr)
    inline def asFE[T <: DFTypeAny]: T = new DFType(dfErr).asInstanceOf[T]
    inline def asValOf[T <: DFTypeAny]: DFValOf[T] = DFVal[T, ModifierAny, DFError](dfErr)
    inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] = DFVal[T, M, DFError](dfErr)
    inline def asOwner: DFOwnerAny = DFOwner[ir.DFOwner](dfErr)
end DFError

class DFWarning(
    val opName: String,
    val dfMsg: String
)(using dfc: DFC) extends LogEvent derives CanEqual

class Logger:
  def logEvent(event: LogEvent): Unit = ???
  def injectEvents(fromLogger: Logger): Unit = ???
  def injectEvents(newEvents: List[LogEvent]): Unit = ???
  def getErrors: List[DFError] = ???
  def getWarnings: List[DFWarning] = ???
  def getEvents: List[LogEvent] = ???
  def clearEvents(): Unit = ???

def trydfSpecific[T](
    block: => T
)(finale: DFError => T)(using dfc: DFC, ctName: CTName): T = ???

@targetName("tryDFType")
@metaContextForward(0)
def trydf[T <: DFTypeAny](block: => T)(using DFC, CTName): T =
  trydfSpecific(block)(dfErr => new DFTypeAny(dfErr).asInstanceOf[T])

@targetName("tryDFVal")
@metaContextForward(0)
def trydf[V <: DFValAny](block: => V)(using DFC, CTName): V =
  trydfSpecific(block)(_.asVal[DFTypeAny, ModifierAny].asInstanceOf[V])

@targetName("tryDFNet")
@metaContextForward(0)
def trydf(block: => Unit)(using DFC, CTName): Unit =
  trydfSpecific(block)(_ => ())

@targetName("tryDFOwner")
@metaContextForward(0)
def trydf[V <: DFOwnerAny](block: => V)(using DFC, CTName): V =
  trydfSpecific(block)(_.asOwner.asInstanceOf[V])

def exitWithError(msg: String)(using DFC): Nothing = ???
