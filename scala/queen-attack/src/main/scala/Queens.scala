object Queens {
  def apply() = new Queens
}

class Queens {
  
  def boardString(white: Option[Position], black: Option[Position]): String = ???

  def canAttack(p1: Position, p2: Position): Boolean = ???
}

case class Position(x: Int, y: Int)
