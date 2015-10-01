
object Garden {
  val default = List(
    "Alice", "Bob", "Charlie", "David",
    "Eve", "Fred", "Ginny", "Harriet",
    "Ileana", "Joseph", "Kincaid", "Larry"
  )

  def defaultGarden(garden: String): Garden = Garden(default, garden)
}

case class Garden(private val _names: List[String], private val _garden: String) {

  import Plant.Plant

  type Row = List[String]

  private val names = _names.sorted
  private val namedGarden: List[Row] = { // first row has names
    names +: _garden.split( """\n""").map(row => row.grouped(2).toList).toList
  }

  def getPlants(name: String): List[Plant] = {
    def find(garden: List[Row]): List[Plant] = garden match {
      case List(n :: ns, p1 :: row1, p2 :: row2) if n == name => (p1 + p2).map(c => Plant.withName(c.toString)).toList
      case List(_ :: ns, _ :: row1, _ :: row2)                => find(List(ns, row1, row2))
      case List(_, Nil, Nil)                                  => List.empty[Plant]

    }
    find(namedGarden)
  }
}

object Plant extends Enumeration {
  type Plant = Value
  val Grass = Value("G")
  val Clover = Value("C")
  val Radishes = Value("R")
  val Violets = Value("V")
}
