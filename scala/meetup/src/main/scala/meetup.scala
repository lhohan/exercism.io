import java.util.Calendar._
import java.util.{Calendar, GregorianCalendar}

object Meetup {
  val Mon = MONDAY
  val Tue = TUESDAY
  val Wed = WEDNESDAY
  val Thu = THURSDAY
  val Fri = FRIDAY
  val Sat = SATURDAY
  val Sun = SUNDAY
}

case class Meetup(m: Int, y: Int) {

  private val jMonth = m - 1

  private def daysFrom(day: GregorianCalendar): Stream[GregorianCalendar] = day #:: daysFrom(nextDay(day))

  private def days: Stream[GregorianCalendar] = daysFrom(new GregorianCalendar(y, jMonth, 1)).takeWhile(_.get(MONTH) == jMonth)


  private def nextDay(calendar: Calendar) = {
    new GregorianCalendar(calendar.get(YEAR), calendar.get(MONTH), calendar.get(DATE) + 1)
  }

  private def daysOfWeekday(weekDay: Int): Stream[GregorianCalendar] = days.filter(_.get(DAY_OF_WEEK) == weekDay)

  private def ith(weekDay: Int, i: Int) = daysOfWeekday(weekDay).drop(i - 1).head

  def teenth(dayOfWeek: Int): GregorianCalendar = {
    daysOfWeekday(dayOfWeek).filter(_.get(DAY_OF_MONTH) / 10 == 1).last
  }

  def first(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 1)

  def second(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 2)

  def third(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 3)

  def fourth(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 4)

  def last(dayOfWeek: Int): GregorianCalendar = daysOfWeekday(dayOfWeek).last

}

