package brainfuck

case class State(dataPointer: Int, buffer: Array[Int], output: List[Int])
