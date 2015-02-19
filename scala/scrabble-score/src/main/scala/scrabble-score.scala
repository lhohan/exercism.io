class Scrabble() {

  import Scrabble._

  def scoreLetter(char: Char) = scoreForLetter(char.toUpper)

  def scoreWord(word: String) = word.map(scoreLetter).sum

}

object Scrabble {

  private val scores = Map(
    "A, E, I, O, U, L, N, R, S, T" -> 1,
    "D, G " -> 2,
    "B, C, M, P" -> 3,
    "F, H, V, W, Y" -> 4,
    "K" -> 5,
    "J, X" -> 8,
    "Q, Z" -> 10
  )

  val scoreForLetter: Map[Char, Int] = for {
    (letters, score) <- scores
    letter <- letters.split(",")
  } yield (letter.trim.head, score)

}