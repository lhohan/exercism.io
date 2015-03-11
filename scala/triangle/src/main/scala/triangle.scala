object TriangleType extends Enumeration {
  val Illogical, Scalene, Isosceles, Equilateral = Value
}

case class Triangle(x: Int, y: Int, z: Int) {
  def triangleType =
    if (x + y <= z || x + z <= y || y + z <= x) {
      TriangleType.Illogical
    } else if (x == y && y == z) {
      TriangleType.Equilateral
    } else if (x == y || y == z || x == z) {
      TriangleType.Isosceles
    } else {
      TriangleType.Scalene
    }
}


