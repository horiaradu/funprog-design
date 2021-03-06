package week4

//class BankAccount {
//  private var balance = 0
//
//  def deposit(amount: Int): Unit =
//    if (amount > 0) {
//      balance += amount
//    }
//
//  def withdraw(amount: Int): Unit =
//    if (0 < amount && amount <= balance) {
//      balance -= amount
//    } else {
//      throw new Error("insufficient funds")
//    }
//}
class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val current = balance()
      balance() = current + amount
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val current = balance()
      balance() = current - amount
    } else {
      throw new Error("insufficient funds")
    }
}