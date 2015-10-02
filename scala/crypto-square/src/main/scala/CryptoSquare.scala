object CryptoSquare {

  def apply() = this

  def normalizedCiphertext(s: String): String = transpose(s).mkString(" ")

  def ciphertext(s: String): String = transpose(s).mkString("")

  private def transpose(s: String): List[String] = {
    def transpose(m: List[String], acc: List[String]): List[String] = {
      if (m.flatten.isEmpty) {
        acc
      } else {
        val strippedColumn = m.foldLeft((List.empty[Char], List.empty[String])) {
          (acc, c) =>
            val (cs, ss) = acc
            c.toCharArray.toList match {
              case Nil               => (cs, ss)
              case (h: Char) :: rest => (cs :+ h, ss :+ rest.mkString(""))
            }
        }
        transpose(strippedColumn._2, acc :+ strippedColumn._1.mkString(""))
      }
    }
    transpose(plaintextSegments(s), Nil)
  }

  def plaintextSegments(s: String): List[String] = {
    val normalized = normalizePlaintext(s)
    if (normalized.isEmpty) {
      List("")
    } else {
      val size = squareSize(normalized)
      normalized.grouped(size).toList
    }
  }

  def squareSize(s: String): Int = squares.dropWhile { case (i, x) => x < s.length }.head._1

  def normalizePlaintext(s: String): String = s.filter(_.isLetterOrDigit).toLowerCase

  private val squares: Stream[(Int, Int)] = Stream.from(0).map { x => (x, x * x) }


}
