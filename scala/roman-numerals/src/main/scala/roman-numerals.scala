import scala.annotation.tailrec

case class RomanNumeral(r: Int) {

  import RomanNumeral._

  type DigitRoman = (Int, Char)

  case class RomanRange(lower: DigitRoman, upper: DigitRoman, lowerOfLower: Option[DigitRoman] = None)

  def convert(x: Int, range: RomanRange): String = {
    @tailrec
    def convertLoop(remaining: Int, acc: String, nums: List[Int]): String =
      (remaining, nums) match {
        case (0, _)                                      =>
          acc
        case (_, Nil)                                    =>
          throw new RuntimeException("no more numbers")
        case (r, n1 :: n2 :: n3 :: ns) if (n1 - n3) == r =>
          convertLoop(r - (n1 - n3), acc + s"${digiToRoman(n3)}${digiToRoman(n1)}", ns)
        case (r, n1 :: n2 :: ns) if n2 == r              =>
          convertLoop(0, acc + s"${digiToRoman(r)}", ns)
        case (r, n1 :: n2 :: ns) if (n1 - n2) == r       =>
          convertLoop(r - (n1 - n2), acc + s"${digiToRoman(n2)}${digiToRoman(n1)}", ns)
        case (r, n :: ns) if (r - n) < 0                 =>
          convertLoop(r, acc, ns)
        case (r, n :: ns) if (r - n) == 0                =>
          convertLoop(r - n, acc + digiToRoman(n), ns)
        case (r, n :: ns) if (r - n) > 0                 =>
          convertLoop(r - n, acc + digiToRoman(n), n :: ns)
      }
    convertLoop(x, "", digiDesc)
  }

  private def toRoman: Int => String = x => {

    @tailrec
    def findRange(x: Int, romans: List[DigitRoman], boundaries: RomanRange): RomanRange = {
      val upper = boundaries.upper
      if (x > 1000) {
        RomanRange((Int.MaxValue, '-'), romans.head, Some(romans.drop(1).head))
      } else
      if (x <= upper._1) {
        boundaries
      } else {
        findRange(x, romans.tail, RomanRange(upper, romans.head, Some(boundaries.lower)))
      }
    }

    convert(x, findRange(x, digiToRomanSorted, RomanRange(digiToRomanSorted.head, digiToRomanSorted.drop(1).head)))
  }

  val value = {
    // 576 -> (500, 70, 6)
    def digits(int: Int) = {
      @tailrec
      def pow(x: Int, y: Int, t: Int = 1): Int = if (y == 0) {
        t
      } else {
        pow(x, y - 1, t * x)
      }
      val digits = int.toString.toList
      digits.zip((0 until digits.length).reverse).map { case (c, d) => c.asDigit * pow(10, d)}
    }

    if (r == 0) {
      ""
    }
    else {
      digits(r).map(toRoman).mkString
    }

  }

}

object RomanNumeral {
  val romanToDigi = Map(
    'I' -> 1, 'V' -> 5, 'X' -> 10,
    'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000
  )
  val digiToRoman = romanToDigi.map(_.swap)
  // List((1,I), (5,V), (10,X), (50,L), (100,C), (500,D), (1000,M))
  val digiToRomanSorted = digiToRoman.toList.sorted
  val digiDesc = digiToRomanSorted.map(_._1).reverse
}