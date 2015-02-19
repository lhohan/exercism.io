object Grains {

  def square(s: Int): BigInt = BigInt(2).pow(s - 1)

  lazy val total: BigInt = (1 to 64).foldLeft(BigInt(0))((acc, i) => acc + square(i))

}