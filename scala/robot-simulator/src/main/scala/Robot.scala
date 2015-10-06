import Bearing.Bearing

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  def simulate(s: String): Robot = s.foldLeft(this) { (robot, action) =>
    action match {
      case 'A' => robot.advance
      case 'L' => robot.turnLeft()
      case 'R' => robot.turnRight
    }
  }

  def turnLeft(): Robot = Robot(bearing.left, coordinates)

  def turnRight: Robot = Robot(bearing.right, coordinates)

  def advance: Robot = bearing match {
    case Bearing.North => Robot(bearing, coordinates.copy(_2 = coordinates._2 + 1))
    case Bearing.East  => Robot(bearing, coordinates.copy(_1 = coordinates._1 + 1))
    case Bearing.South => Robot(bearing, coordinates.copy(_2 = coordinates._2 - 1))
    case Bearing.West  => Robot(bearing, coordinates.copy(_1 = coordinates._1 - 1))
  }

}

object Bearing {

  sealed trait Bearing {
    def left: Bearing

    def right: Bearing
  }

  case object North extends Bearing {
    
    override def left: Bearing = West

    override def right: Bearing = East
  }

  case object East extends Bearing {
    
    override def left: Bearing = North

    override def right: Bearing = South
  }

  case object South extends Bearing {
    
    override def left: Bearing = East

    override def right: Bearing = West
  }

  case object West extends Bearing {
    
    override def left: Bearing = South

    override def right: Bearing = North
  }

}
