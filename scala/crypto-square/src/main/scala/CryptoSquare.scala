object CryptoSquare {

  def apply() = this

  def normalizedCiphertext(s: String): String = transpose(s).mkString(" ")

  def ciphertext(s: String): String = transpose(s).mkString("")

  private def transpose(s: String): List[String] = {
    def transpose(m: List[String], acc: List[String]): List[String] = {
      if (m.flatten.isEmpty) {
        acc
      } else {
        val firstColToRow = m.foldLeft(List.empty[Char]) {
          (acc, c) =>
            c.toCharArray.toList match {
              case Nil               => acc
              case (h: Char) :: rest => acc :+ h
            }
        }
        val withoutFirstCol = m.foldLeft(List.empty[String]) {
          (acc, c) => c.toCharArray.toList match {
            case Nil               => acc
            case (_: Char) :: rest => acc :+ rest.mkString("")
          }
        }
        transpose(withoutFirstCol, acc :+ firstColToRow.mkString(""))
      }
    }
    transpose(plaintextSegments(s), Nil)
  }

  def plaintextSegments(s: String): List[String] = {
    val normalized = normalizePlaintext(s)
    val size = squareSize(normalized)

    if (size == 0) {
      List("")
    } else {
      normalized.grouped(size).toList
    }
  }

  def squareSize(s: String): Int = squares.dropWhile { case (i, x) => x < s.length }.head._1

  def normalizePlaintext(s: String): String = s.filter(_.isLetterOrDigit).toLowerCase

  private val squares: Stream[(Int, Int)] = Stream.from(0).map { x => (x, x * x) }


}
