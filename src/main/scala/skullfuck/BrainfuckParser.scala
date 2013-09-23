/**
 * Copyright © 2013, Adam Retter
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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
