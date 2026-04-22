package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.quoted.*
import annotation.targetName

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
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
    type OutI = 1
  given fromDFBitsInt[W <: Int]: Width[DFBits[W]] with
    type Out = W
    type OutI = W
  given fromDFDecimalInt[S <: Boolean, W <: Int, F <: Int, N <: ir.DFDecimal.NativeType]
      : Width[DFDecimal[S, W, F, N]] with
    type Out = W
    type OutI = W
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  object Success extends Width[Any]
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    '{ Success.asInstanceOf[Width[T] { type Out = Int; type OutI = Int }] }
end Width

