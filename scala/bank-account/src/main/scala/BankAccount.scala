case class BankAccount(private var balance: Option[Int] = Some(0)) {

  def getBalance: Option[Int] = balance

  def incrementBalance(amount: Int): Option[Int] = {
    this.synchronized {
      balance = balance.map(_ + amount)
    }
    balance
  }

  def closeAccount() = {
    this.synchronized {
      balance = None
    }
  }
}
