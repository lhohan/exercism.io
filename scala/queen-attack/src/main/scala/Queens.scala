object Queens {
  def apply() = new Queens
}

class Queens {

  val EmptyBoard: List[List[Char]] = List.fill(8, 8)('_')

  def boardString(white: Option[Position], black: Option[Position]): String = {
    def update(board: List[List[Char]], maybePosition: Option[Position], color: Char): List[List[Char]] = {
      maybePosition match {
        case Some(p) => board.updated(p.x, board(p.x).updated(p.y, color))
        case None    => board
      }
    }

    val board = update(update(EmptyBoard, white, 'W'), black, 'B')
    board.map(_.mkString("", " ", "\n")).mkString("")
  }

  private def abs(x: Int) = if (x < 0) -x else x

  def canAttack(p1: Position, p2: Position): Boolean = (p1, p2) match {
    case (Position(x1, _), Position(x2, _)) if x1 == x2                       => true
    case (Position(_, y1), Position(_, y2)) if y1 == y2                       => true
    case (Position(x1, y1), Position(x2, y2)) if abs(x1 - x2) == abs(y1 - y2) => true
    case _                                                                    => false
  }
}

case class Position(x: Int, y: Int)
