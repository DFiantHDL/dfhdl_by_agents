package dfhdl.core
final class BoolSelWrapper[SP, OT, OF](
    val sel: DFValTP[DFBoolOrBit, SP],
    val onTrue: OT,
    val onFalse: OF
)
object BoolSelWrapper
