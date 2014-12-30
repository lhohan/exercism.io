import scala.collection.immutable.SortedMap

class School {
  private var map = SortedMap[Int, List[String]]()

  def db = map.toMap

  def sorted = db.mapValues(_.sorted)

  def add(name:String, g:Int): Unit =
    map = map.updated(g, grade(g) :+ name)

  def grade(g:Int) =
    map.getOrElse(g, List.empty[String])
}