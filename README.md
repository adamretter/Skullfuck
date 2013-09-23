Skullfuck
=========

Interpreter for the [Brainfuck programming language](http://www.muppetlabs.com/~breadbox/bf/) written in Scala.

Developed as part of the 3rd West London Hack Night on August 15th 2013.
http://www.meetup.com/West-London-Hack-Night/events/130356642/

Initial hack was by Adam Retter and Dylan Lucas, and subsequently completed
by Adam.

A nice feature is that this Interpreter if completely functional and only
uses immutable variables. The generation of the AST is done by using
Scala's Parser Combinators and the evaluation of the AST is just a left-fold.


Implementation Specifics
------------------------
* Each cell is 8 bits, e.g. 1 byte in the JVM.

* Cells wrap around, i.e. adding to the maximum cell value brings it back to zero.

* The array of Cells is extended to the right indefinetly, i.e. there is no limit of 30,000 cells (although a warning message is logged).

* The array of Cells is not extended to the left.

* Trying to move the data pointer before zero is a no-op, and a warning message is logged.
