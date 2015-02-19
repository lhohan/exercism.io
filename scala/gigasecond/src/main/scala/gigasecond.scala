import java.util.Calendar

case class Gigasecond(cal: Calendar) {
  def date = {
    val c = cal.clone().asInstanceOf[Calendar]
    c.add(Calendar.SECOND, 1000000000)
    c
  }
}