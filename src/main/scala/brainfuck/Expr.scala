package brainfuck

abstract class Expr {
  def eval(state: State) : State = state
}

case object Increment extends Expr {
  override def eval(state: State): State = state.copy(dataPointer = state.dataPointer + 1)
}


case object Decrement extends Expr {
  override def eval(state: State): State = state.copy(dataPointer = state.dataPointer - 1)
}

case object IncrementData extends Expr {
  override def eval(state: State) = {

    var buf = state.buffer
    if(buf.isEmpty) {
      buf = Array[Int](1)
    } else {
      buf(state.dataPointer) = buf(state.dataPointer) + 1
    }

    state.copy(buffer = buf)
  }
}

case object OutputData extends Expr {
  override def eval(state: State): State = {
    val outInt = state.buffer(state.dataPointer)
    state.copy(output = outInt :: state.output)
  }
}