import scala.util.Random

class Robot {

  private var current = newName

  def name = current

  def reset(): Unit = {
    current = newName
  }

  private def newName = {
    val r = new Random()
    val chars = r.alphanumeric.filter(_.isLetter).take(2).toList.mkString("")
    val nums = r.alphanumeric.filter(_.isDigit).take(3).toList.mkString("")
    chars + nums
  }

}