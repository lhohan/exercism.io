object Atbash {
  def apply() = this


  private val atbash = {
    val letters = 'a' to 'z'
    val digits = (0 to 9).map(d => ('0' + d).toChar)
    val atbashLetterMapping = letters.zip(letters.reverse)
    val digitMapping: IndexedSeq[(Char, Char)] = digits.zip(digits)

    (atbashLetterMapping ++ digitMapping).toMap
  }

  def encode(s: String): String = {
    def isSupportedChar: (Char) => Boolean = atbash.keySet.contains
    def format(a: String): String = a.grouped(5).mkString(" ")

    val encoded = s.toLowerCase.filter(isSupportedChar).map(atbash)
    format(encoded)
  }
}
