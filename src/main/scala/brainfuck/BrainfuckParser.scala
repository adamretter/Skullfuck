package brainfuck

import scala.util.parsing.combinator.RegexParsers

class BrainfuckParser extends RegexParsers {

  def increment_p = ">".r ^^ { x =>
    Increment
  }
  def decrement_p = "<".r ^^ { x =>
    Decrement
  }

  def increment_data = "\\+".r ^^ { x =>
     IncrementData
  }

  def output_data = "\\.".r ^^ { x =>
    OutputData
  }

  def expr = (increment_p | decrement_p | increment_data | output_data)+

  case class StartExpr(startState: State) extends Expr {
    override def eval(state: State): State = startState
  }

  def apply(input: String) : String = {
    parseAll(expr, input) match {
      case Success(result, _) =>

        val initialState = State(0, Array.empty[Int], List.empty[Int])

        val finalState = result.foldLeft(initialState)((a,b) => b.eval(a))
        finalState.output.toString

      case failure : NoSuccess =>
        scala.sys.error(failure.msg)
    }
  }
}
