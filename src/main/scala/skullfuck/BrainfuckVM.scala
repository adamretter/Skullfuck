package skullfuck

object  BrainfuckVM extends App {
  val vm = new BrainfuckParser()

  val result = vm.apply("+++.>+++.")

  println(result)

}
