package brainfuck

import scala.util.parsing.combinator.RegexParsers

class BrainfuckParser extends RegexParsers {

  def increment_p = ">".r ^^ { x =>
    IncrementDataPointer
  }
  def decrement_p = "<".r ^^ { x =>
    DecrementDataPointer
  }

  def increment_data = "\\+".r ^^ { x =>
     IncrementCell
  }

  def decrement_data = "\\-".r ^^ { x =>
    DecrementCell
  }

  def output_data = "\\.".r ^^ { x =>
    OutputData
  }

  def expr = (increment_p | decrement_p | increment_data | decrement_data | output_data)+

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
