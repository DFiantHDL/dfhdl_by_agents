package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName

sealed abstract class DFEncoding extends scala.reflect.Enum:
  def calcWidth(entryCount: Int): Int
  def encode(idx: Int): BigInt
  def bigIntValue: BigInt

object DFEncoding:
  sealed trait Auto extends DFEncoding:
    final val bigIntValue: BigInt = ???
  sealed trait ExplicitWidth[W <: Int & Singleton] extends DFEncoding:
    val width: W
  abstract class Default extends StartAt(0)

  abstract class Gray extends Auto:
    final def calcWidth(entryCount: Int): Int = ???
    final def encode(idx: Int): BigInt = ???

  abstract class StartAt[V <: Int & Singleton](value: V) extends Auto:
    final def calcWidth(entryCount: Int): Int = ???
    final def encode(idx: Int): BigInt = ???

  abstract class OneHot extends Auto:
    final def calcWidth(entryCount: Int): Int = ???
    final def encode(idx: Int): BigInt = ???

  abstract class Manual[W <: Int & Singleton](val width: W) extends ExplicitWidth[W]:
    val value: DFConstOf[DFUInt[W]]
    final def bigIntValue: BigInt = ???
    final def calcWidth(entryCount: Int): Int = ???
    final def encode(idx: Int): BigInt = ???

  abstract class Toggle extends Default, ExplicitWidth[1] derives CanEqual:
    val width: 1 = 1

end DFEncoding

type DFEnum[E <: DFEncoding] = DFType[ir.DFEnum, Args1[E]]
object DFEnum:
  def unapply(using
      Quotes
  )(
      tpe: quotes.reflect.TypeRepr
  ): Option[List[quotes.reflect.TypeRepr]] = ???
  def apply[E <: DFEncoding](enumCompanion: Object): DFEnum[E] = ???

  inline given [E <: DFEncoding]: DFEnum[E] = ${ dfTypeMacro[E] }
  def dfTypeMacro[E <: DFEncoding](using Quotes, Type[E]): Expr[DFEnum[E]] = ???

  object Val:
    object TC:
      import DFVal.TC
      given DFEnumFromEntry[E <: DFEncoding, RE <: E]: TC[DFEnum[E], RE] with
        type OutP = CONST
        def conv(dfType: DFEnum[E], value: RE)(using DFC): Out = ???
    object Compare:
      import DFVal.Compare
      given DFEnumCompareEntry[
          E <: DFEncoding, RE <: E,
          Op <: FuncOp.===.type | FuncOp.=!=.type, C <: Boolean
      ]: Compare[DFEnum[E], RE, Op, C] with
        type OutP = CONST
        def conv(dfType: DFEnum[E], arg: RE)(using DFC): Out = ???
    object Ops
  end Val
end DFEnum
