object Grains {

  private val powersOf2 = Stream.from(1).map(i => BigInt(2).pow(i - 1))

  def square(s: Int): BigInt = powersOf2.take(s).last

  lazy val total: BigInt = 1.to(64).map(square).sum
}