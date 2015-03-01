import scala.util.Random

class Robot {

  import Robot._

  private var current = newName

  def name = current

  def reset(): Unit = current = newName

}


object Robot {

  private def newName: String = allNames.headOption match {
    case Some(n) => n
    case None => throw new UnsupportedOperationException("No more names available")
  }

  // lazy list of all names, shuffled
  // added the type to clarify
  private def allNames: Stream[String] = Random.shuffle(
    for {
      l1 <- letters
      l2 <- letters
      d1 <- digits
      d2 <- digits
      d3 <- digits
    } yield s"$l1$l2$d1$d2$d3")

  private def letters = ('A' to 'Z').toStream

  private def digits = (0 to 9).toStream

}