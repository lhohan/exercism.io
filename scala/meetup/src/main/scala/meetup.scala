import java.util.{Calendar, GregorianCalendar}

object Meetup extends Enumeration {
  type DayOfWeek = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

case class Meetup(m: Int, y: Int) {

  private def toJavaMonth(m: Int) = m - 1

  private def days: Stream[GregorianCalendar] = daysFrom(new GregorianCalendar(y, toJavaMonth(m), 1)).takeWhile(c => c.get(Calendar.MONTH) == toJavaMonth(m))

  private def daysFrom(day: GregorianCalendar): Stream[GregorianCalendar] = day #:: daysFrom(nextDay(day))

  private def nextDay(calendar: Calendar) = {
    val next = new GregorianCalendar()
    next.setTime(calendar.getTime)
    next.add(Calendar.DAY_OF_MONTH, 1)
    next
  }

  private def daysOfWeekday(dow: Int): Stream[GregorianCalendar] = days.filter(_.get(Calendar.DAY_OF_WEEK) == dow)

  private def daysOfWeekday(dow: Meetup.DayOfWeek): Stream[GregorianCalendar] = daysOfWeekday(convert(dow))

  private def convert(dow: Meetup.DayOfWeek): Int = dow match {
    case Meetup.Mon => Calendar.MONDAY
    case Meetup.Tue => Calendar.TUESDAY
    case Meetup.Wed => Calendar.WEDNESDAY
    case Meetup.Thu => Calendar.THURSDAY
    case Meetup.Fri => Calendar.FRIDAY
    case Meetup.Sat => Calendar.SATURDAY
    case Meetup.Sun => Calendar.SUNDAY
  }

  private def ith(dayOfWeek: Meetup.DayOfWeek, i: Int) = daysOfWeekday(dayOfWeek).drop(i - 1).head

  def teenth(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = {
    daysOfWeekday(dayOfWeek).filter(_.get(Calendar.DAY_OF_MONTH) / 10 == 1).last
  }

  def first(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = ith(dayOfWeek, 1)

  def second(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = ith(dayOfWeek, 2)

  def third(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = ith(dayOfWeek, 3)

  def fourth(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = ith(dayOfWeek, 4)

  def last(dayOfWeek: Meetup.DayOfWeek): GregorianCalendar = daysOfWeekday(dayOfWeek).last

}

