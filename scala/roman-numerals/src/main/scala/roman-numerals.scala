import scala.annotation.tailrec

case class RomanNumeral(r: Int) {

  import RomanNumeral._

  type DigitRoman = (Int, Char)

  case class RomanRange(lower: DigitRoman, upper: DigitRoman, lowerOfLower: Option[DigitRoman] = None)

  //  @tailrec
  def convert(x: Int, range: RomanRange): String = {
    @tailrec
    def expandedRoman(x: Int, acc: String, digits: List[Int]): String = {
      (x, digits) match {
        case (0, _) => acc
        case (_, Nil) => throw new RuntimeException("no more digits")
        case (xx, d1 :: d2 :: d3 :: ds) if (d1 - d3) == xx => expandedRoman(xx - (d1 - d3), acc + s"${digitToRoman(d3)}${digitToRoman(d1)}", d2 :: d3 :: ds)
        case (xx, d :: ds) if (xx - d) < 0 => expandedRoman(xx, acc, ds)
        case (xx, d :: ds) if (xx - d) == 0 => expandedRoman(xx - d, acc + digitToRoman(d), ds)
        case (xx, d :: ds) if (xx - d) > 0 => expandedRoman(xx - d, acc + digitToRoman(d), d :: ds)
      }
    }
    def collapse(roman:String): String = {
      toCollapse.foldLeft(roman)((acc, x) => acc.replaceAll(x._1, x._2))
    }
    collapse(expandedRoman(x, "", digitsDesc))
  }


  private def digits(int: Int) = {
    @tailrec
    def pow(x: Int, y: Int, t: Int = 1): Int = if (y == 0) {
      t
    } else {
      pow(x, y - 1, t * x)
    }
    val chars = int.toString.toList
    val tuples = chars.zip((0 until chars.length).reverse)
    tuples.map { case (c, d) => c.asDigit * pow(10, d)}
  }

  @tailrec
  private def findRange(x: Int, romans: List[(Int, Char)], boundaries: RomanRange): RomanRange = {
    val upper = boundaries.upper
    if(x > 1000 ) {
      RomanRange((Int.MaxValue, '-'), romans.head, Some(romans.drop(1).head))
    } else
    if (x <= upper._1) {
      boundaries
    } else {
      findRange(x, romans.tail, RomanRange(upper, romans.head, Some(boundaries.lower)))
    }
  }

  private def toRoman: Int => String = x => {
    val range = findRange(x, dtrSorted, RomanRange(dtrSorted.head, dtrSorted.drop(1).head))

    convert(x, range)
  }

  val value = {
    if (r == 0) {
      ""
    }
    else {
      val result = digits(r).map(toRoman)
      println(result)
      result.mkString
    }

  }

}

object RomanNumeral {
  val romanToDigit = Map(
    'I' -> 1, 'V' -> 5, 'X' -> 10,
    'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000
  )
  val digitToRoman = romanToDigit.map(_.swap)
  // List((1,I), (5,V), (10,X), (50,L), (100,C), (500,D), (1000,M))
  val dtrSorted = digitToRoman.toList.sorted
  val digitsDesc = dtrSorted.map(_._1).reverse
  val toCollapse = (dtrSorted zip dtrSorted.tail).map{case (dtr1, dtr2) => (dtr1._2, dtr2._2)}.map{case (r1, r2) => (List.fill(4)(r1).mkString,s"$r1$r2")}
  println(toCollapse)
}