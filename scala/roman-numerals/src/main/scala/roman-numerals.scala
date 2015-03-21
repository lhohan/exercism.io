import scala.annotation.tailrec

case class RomanNumeral(number: Int) {

  import RomanNumeral._

  case class RomanRange(lower: NumberRoman, upper: NumberRoman, lowerOfLower: Option[NumberRoman] = None)

  private def toRoman: Int => String = x => {

    @tailrec
    def findRange(x: Int, romans: List[NumberRoman], boundaries: RomanRange): RomanRange = {
      require(x >= 0, s"Illegal roman number $x")
      val upper = boundaries.upper
      if (x > 1000) {
        RomanRange((Int.MaxValue, '-'), romans.head, Some(romans.drop(1).head))
      } else if (x == 0) {
        RomanRange((0, '-'), (0, '-'), None)
      } else if (x <= upper._1) {
        boundaries
      } else {
        findRange(x, romans.tail, RomanRange(upper, romans.head, Some(boundaries.lower)))
      }
    }

    def convert(x: Int, range: RomanRange): String = {
      @tailrec
      def convertLoop(remaining: Int, acc: String, numbers: List[Int]): String =
        (remaining, numbers) match {
          case (0, _)                                     =>
            acc
          case (_, Nil)                                   =>
            throw new RuntimeException("no more numbers")
          case (r, n1 :: _ :: n3 :: ns) if (n1 - n3) == r => // 9 = 10 - 1 = IX , numbers = [10, 5, 1]
            convertLoop(r - (n1 - n3), acc + s"${roman(n3)}${roman(n1)}", ns)
          case (r, _ :: n2 :: ns) if n2 == r              => // 5 = V not VX 
            convertLoop(0, acc + s"${roman(r)}", ns)
          case (r, n1 :: n2 :: ns) if (n1 - n2) == r      => // 40 = 50 - 10 = XL , numbers [50, 10, 5, 1]
            convertLoop(r - (n1 - n2), acc + s"${roman(n2)}${roman(n1)}", ns)
          case (r, n :: ns) if (r - n) < 0                =>
            convertLoop(r, acc, ns)
          case (r, n :: ns) if (r - n) == 0               =>
            convertLoop(r - n, acc + roman(n), ns)
          case (r, n :: ns) if (r - n) > 0                =>
            convertLoop(r - n, acc + roman(n), n :: ns)
        }
      convertLoop(x, "", numbersDesc)
    }

    convert(x, findRange(x, sortedNumberRomans, RomanRange(sortedNumberRomans.head, sortedNumberRomans.drop(1).head)))
  }

  val value = {
    // 576 -> (500, 70, 6)
    def split(int: Int) = {
      @tailrec
      def pow(x: Int, y: Int, t: Int = 1): Int = if (y == 0) {
        t
      } else {
        pow(x, y - 1, t * x)
      }

      val digits = int.toString.toList
      digits.zip((0 until digits.length).reverse).map { case (d, i) => d.asDigit * pow(10, i)}
    }

    split(number).map(toRoman).mkString
  }

}

object RomanNumeral {
  type NumberRoman = (Int, Char)
  val romanToNumbers = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
  val roman = romanToNumbers.map(_.swap)
  // List((1,I), (5,V), (10,X), (50,L), (100,C), (500,D), (1000,M))
  val sortedNumberRomans: List[NumberRoman] = roman.toList.sorted
  val numbersDesc = sortedNumberRomans.map(_._1).reverse
}