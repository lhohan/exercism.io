import scala.util.Random

class Robot {

  import Robot._

  private var current = newName

  def name = current

  def reset(): Unit = current = newName

}


object Robot {

  private def newName: String = randomNames.head // when exhausted will throw exception

  private def randomNames = Random.shuffle( // lazy list of all names, does not contain duplicates
    for {
      l1 <- letters
      l2 <- letters
      d1 <- digits
      d2 <- digits
      d3 <- digits
    } yield s"$l1$l2$d1$d2$d3")

  private def letters = 'A' to 'Z'
  private def digits = 0 to 9

}