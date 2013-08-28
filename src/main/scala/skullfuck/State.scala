package skullfuck

/**
 * @param ptrData A pointer into the data, i.e. The Data Pointer
 * @param data An array of data cells
 */
case class State(ptrData: Int, data: Array[Byte], output: List[Int])
