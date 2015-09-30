object Atbash {
  def apply() = this

  val letters = 'a' to 'z'
  val digits = (0 to 9).map(d => ('0' + d).toChar)
  val encoder = letters.zip(letters.reverse).toMap ++ digits.zip(digits).toMap

  def encode(s: String): String = {
    s.toLowerCase.filter(encoder.keySet.contains).map(encoder).grouped(5).mkString(" ")
  }
}
