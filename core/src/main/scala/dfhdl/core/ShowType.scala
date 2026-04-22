package dfhdl.core
import scala.quoted.*

extension [T](using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def showTuple(showf: quotes.reflect.TypeRepr => String): List[String] = ???
  def showDFType: String = ???
  def showModifier: String = ???
  def showDFVal: String = ???
  def showType: String = ???

trait ShowType[T]:
  type Out <: String
object ShowType:
  transparent inline given [T]: ShowType[T] = ???
