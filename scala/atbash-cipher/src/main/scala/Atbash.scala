object Atbash {
  def apply() = this

  val Letters = 'a' to 'z'
  val AtbashMapping = Letters.zip(Letters.reverse).toMap

  private def atbash: PartialFunction[Char, Char] = {
    case c if c.isLetter => AtbashMapping(c)
    case c if c.isDigit  => c
  }

  def encode(s: String): String = {
    def format(a: String): String = a.grouped(5).mkString(" ")

    val encoded = s.toLowerCase.collect(atbash)
    format(encoded)
  }
}
