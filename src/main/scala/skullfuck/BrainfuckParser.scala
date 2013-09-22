package skullfuck

import scala.util.parsing.combinator.RegexParsers

class BrainfuckParser extends RegexParsers {

  def increment_ptr = ">".r ^^^
    IncrementDataPointer

  def decrement_ptr = "<".r ^^^
    DecrementDataPointer

  def increment_data = """\+""".r ^^^
     IncrementCell

  def decrement_data = """\-""".r ^^^
    DecrementCell

  def output_data = """\.""".r ^^^
    OutputData

  def jump_if : Parser[JumpIf] = """\[""".r ~> expr <~ """\]""".r ^^ { exprs =>
    JumpIf(exprs)
  }

  def expr = (increment_ptr | decrement_ptr | increment_data | decrement_data | output_data | jump_if)+

  case class StartExpr(startState: State) extends Expr {
    override def eval(state: State): State = startState
  }

  def apply(input: String) : String = {
    parseAll(expr, input) match {
      case Success(result, _) =>

        val initialState = State(0, Array.empty[Byte], List.empty[Int])

        val finalState = result.foldLeft(initialState)((a,b) => b.eval(a))
        finalState.output.toString

      case failure : NoSuccess =>
        scala.sys.error(failure.msg)
    }
  }
}
