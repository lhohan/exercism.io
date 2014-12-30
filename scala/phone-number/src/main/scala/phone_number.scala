class PhoneNumber(n: String) {

  val BadNumber = "0000000000"

  lazy val number = n.filter(_.isDigit) match {
    case digits if digits.size == 11 && digits.startsWith("1") => digits.tail
    case digits if digits.size == 10 => digits
    case _ => BadNumber
  }

  lazy val areaCode = number.take(3)

  override def toString: String =
    s"($areaCode) ${number.drop(3).take(3)}-${number.takeRight(4)}"
}