import java.util.{Calendar, GregorianCalendar}

object Meetup {
  val Mon = Calendar.MONDAY
  val Tue = Calendar.TUESDAY
  val Wed = Calendar.WEDNESDAY
  val Thu = Calendar.THURSDAY
  val Fri = Calendar.FRIDAY
  val Sat = Calendar.SATURDAY
  val Sun = Calendar.SUNDAY
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

  private def ith(dayOfWeek: Int, i: Int) = daysOfWeekday(dayOfWeek).drop(i - 1).head

  def teenth(dayOfWeek: Int): GregorianCalendar = {
    daysOfWeekday(dayOfWeek).filter(_.get(Calendar.DAY_OF_MONTH) / 10 == 1).last
  }

  def first(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 1)

  def second(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 2)

  def third(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 3)

  def fourth(dayOfWeek: Int): GregorianCalendar = ith(dayOfWeek, 4)

  def last(dayOfWeek: Int): GregorianCalendar = daysOfWeekday(dayOfWeek).last

}

