
case class Year(year: Int) {

  def isLeap: Boolean = year match {
    case y if y % 400 == 0 => true
    case y if y % 100 == 0 => false
    case y if y % 4 == 0 => true
    case _ => false
  }

}