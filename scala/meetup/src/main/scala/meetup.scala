import java.time.DayOfWeek
import java.util.GregorianCalendar

case class Meetup(m: Int, y: Int) {
  def teenth(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
  def first(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
  def second(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
  def third(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
  def fourth(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
  def last(dayOfWeek: Meetup.DayOfWeek):GregorianCalendar = ???
}

object Meetup extends Enumeration {
  type DayOfWeek = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}