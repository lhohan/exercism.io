import scala.util.Random

class Robot {

  import Robot._

  private var current = newName

  def name = current

  def reset(): Unit = current = newName

}

object Robot {

  private var pastNames = Set.empty[String]

  private def randomNames: Stream[String] = randomName #:: randomNames

  private def newName: String = {
    val n = randomNames.filterNot(pastNames.contains).head
    pastNames += n
    n
  }

  private def randomName = letters(2) + digits(3)

  private def letters(n: Int) = new Random().alphanumeric.filter(_.isLetter).take(n).toList.mkString("")

  private def digits(n: Int) = new Random().alphanumeric.filter(_.isDigit).take(n).toList.mkString("")

}