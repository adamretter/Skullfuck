package skullfuck

import grizzled.slf4j.Logger


trait Expr {
  lazy val logger = Logger[this.type]

  def eval(state: State) : State
}

trait ModifyDataPointerExpr extends Expr {
  override def eval(state: State): State = state.copy(ptrData = modifyDataPointer(state.ptrData))
  def modifyDataPointer(ptrData: Int) : Int
}

case object IncrementDataPointer extends ModifyDataPointerExpr {
  override def modifyDataPointer(ptrData: Int) = ptrData + 1
}

case object DecrementDataPointer extends ModifyDataPointerExpr {
  override def modifyDataPointer(ptrData: Int) = {
    if(ptrData > 0)
      ptrData - 1
    else {
      //do not move past left-most cell
      logger.warn("Cannot move past left-most cell, staying at left-most cell!")
      ptrData
    }
  }
}


trait ModifyDataExpr extends Expr {
  def getOrCreateData(state: State) : Array[Byte] = {
    if(state.data.length <= state.ptrData)
      state.data padTo(state.ptrData + 1, 0.asInstanceOf[Byte]) //add more zero'd data cells until we meet the data pointer
    else
      state.data //return the data cells as is
  }

  override def eval(state: State) = {

    val buf = getOrCreateData(state)
    buf(state.ptrData) = modifyCell(buf(state.ptrData))

    state.copy(data = buf)
  }

  def modifyCell(cell: Byte) : Byte
}

case object IncrementCell extends ModifyDataExpr {
  override def modifyCell(cell: Byte): Byte = (cell + 1).asInstanceOf[Byte]
}

case object DecrementCell extends ModifyDataExpr {
  override def modifyCell(cell: Byte): Byte = (cell - 1).asInstanceOf[Byte]
}


case object OutputData extends Expr {
  override def eval(state: State): State = {
    val outInt = state.data(state.ptrData)
    state.copy(output = outInt :: state.output)
  }
}


case class JumpIf(exprs : List[Expr]) extends Expr {
  override def eval(state: State): State = {
    if(state.data(state.ptrData) != 0) {
      val newState = exprs.foldLeft(state)((a,b) => b.eval(a))
      if(newState.data(newState.ptrData) != 0) {
        eval(newState)
      } else {
        newState
      }
    } else {
      state
    }
  }
}