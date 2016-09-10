package week4

object session {

  def main(args: Array[String]): Unit = {
    val initialSig = 4
    val otherSig = 5
    val caller = new StackableVariable(initialSig)
    caller.withValue(otherSig) {
      println(caller.value)
    }
    println(caller.value)

    def consolidated(accounts: List[BankAccount]): Signal[Int] =
      Signal(accounts.map(_.balance()).sum)

    val a = new BankAccount()
    val b = new BankAccount()
    val c = consolidated(List(a, b))

    println(c())

    a.deposit(20)
    println(c())
  }
}