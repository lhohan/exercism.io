import scala.collection.immutable.SortedMap

class School {
  
  type Grade = Int
  type Name = String

  private var _school = SortedMap[Grade, List[Name]]()

  def db = _school.toMap

  def sorted = db.mapValues(_.sorted)

  def add(n: Name, g: Grade): Unit =
    _school = _school.updated(g, grade(g) :+ n)

  def grade(g: Grade) =
    _school.getOrElse(g, List.empty[Name])

}