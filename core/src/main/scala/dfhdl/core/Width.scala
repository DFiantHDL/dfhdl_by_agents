package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import annotation.targetName
import scala.annotation.nowarn

trait Width[T]:
  type Out <: IntP
  type OutI <: Int
trait WidthLP:
  given fromDFBitsIntP[W <: IntP]: Width[DFBits[W]] with
    type Out = W
    type OutI = Int
  given fromDFDecimalIntP[S <: Boolean, W <: IntP, F <: Int, N <: ir.DFDecimal.NativeType]
      : Width[DFDecimal[S, W, F, N]] with
    type Out = W
    type OutI = Int
object Width extends WidthLP:
  type Aux[T, O <: IntP] = Width[T] { type Out = O }
  type AuxI[T, O <: Int] = Width[T] { type OutI = O }
  val wide: Width[DFTypeAny] = new Width[DFTypeAny]:
    type Out = Int
    type OutI = Int
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
    type OutI = 1
  given fromBooleanCompanion: Width[Boolean.type] with
    type Out = 1
    type OutI = 1
  given fromDFDouble: Width[DFDouble] with
    type Out = 64
    type OutI = 64
  given fromDoubleCompanion: Width[Double.type] with
    type Out = 64
    type OutI = 64
  given fromDFBitsInt[W <: Int]: Width[DFBits[W]] with
    type Out = W
    type OutI = W
  given fromDFDecimalInt[S <: Boolean, W <: Int, F <: Int, N <: ir.DFDecimal.NativeType]
      : Width[DFDecimal[S, W, F, N]] with
    type Out = W
    type OutI = W
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  extension (using quotes: Quotes)(dfTpe: quotes.reflect.TypeRepr)
    def +(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = ???
    def *(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = ???
    infix def max(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = ???
    def simplify: quotes.reflect.TypeRepr = ???
    def calcWidth: quotes.reflect.TypeRepr = ???
    def calcValWidth: quotes.reflect.TypeRepr = ???
  end extension
  object Success extends Width[Any]
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    '{ Success.asInstanceOf[Width[T] { type Out = Int; type OutI = Int }] }
end Width

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  @targetName("dfValWidthOpt")
  def widthIntOpt(using dfc: DFC, w: Width[T]): Option[Int] = ???
  def widthIntParam(using dfc: DFC, w: Width[T]): IntParam[w.Out] = ???

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tWidthOpt")
  def widthIntOpt(using dfc: DFC, w: Width[tc.Type]): Option[Int] = ???
  def widthIntParam(using dfc: DFC, w: Width[tc.Type]): IntParam[w.Out] = ???
end extension
