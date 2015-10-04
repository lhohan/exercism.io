import scala.annotation.tailrec

object Garden {

  type Child = String

  private val defaultChildren = List("Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana",
    "Joseph", "Kincaid", "Larry"
  )

  def defaultGarden(garden: String): Garden = Garden(defaultChildren, garden)
}

import Garden.Child

case class Garden(private val _childeren: List[Child], private val _garden: String) {

  import Plant.Plant

  case class Window(p1: Plant, p2: Plant) {
    val plants = List(p1, p2)
  }

  case class GardenRows(r1: GardenRow, r2: GardenRow)

  type Children = List[Child]
  type GardenRow = List[Window]

  implicit def charToPlant(c: Char): Plant = Plant.withName(c.toString)

  private val children: Children = _childeren.sorted
  private val garden: GardenRows = {
    val gardenRows = _garden.split( """\n""").toList
    val gardenWindows = gardenRows.map { row =>
      val pairs = row.grouped(2).toList
      val windows = pairs.map { pair => Window(pair.head, pair.last) }
      windows
    }
    GardenRows(gardenWindows.head, gardenWindows.last)
  }

  def getPlants(child: Child): List[Plant] = {
    @tailrec
    def find(garden: GardenRows, children: Children): List[Plant] = {
      val (row1, row2) = (garden.r1, garden.r2)
      children match {
        case c :: cs if c == child => row1.head.plants ++ row2.head.plants
        case _ :: cs               => (row1, row2) match {
          case (_ :: nextRow1, _ :: nextRow2) => find(GardenRows(nextRow1, nextRow2), cs)
          case _                              => List.empty[Plant]
        }
      }
    }
    find(garden, children)
  }
}

object Plant extends Enumeration {
  type Plant = Value
  val Grass = Value("G")
  val Clover = Value("C")
  val Radishes = Value("R")
  val Violets = Value("V")
}
