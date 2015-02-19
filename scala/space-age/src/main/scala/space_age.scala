import scala.math.BigDecimal.RoundingMode

case class SpaceAge(seconds: Long) {

  lazy val onNeptune = ageRelativeToEarth(164.79132)

  lazy val onUranus = ageRelativeToEarth(84.016846)

  lazy val onSaturn = ageRelativeToEarth(29.447498)

  lazy val onJupiter = ageRelativeToEarth(11.862615)

  lazy val onMars = ageRelativeToEarth(1.8808158)

  lazy val onVenus = ageRelativeToEarth(0.61519726)

  lazy val onMercury = ageRelativeToEarth(0.2408467)

  lazy val onEarth = ageRelativeToEarth(1)

  private lazy val secs = BigDecimal(seconds)

  private def ageRelativeToEarth(factor: Double): Double =
    (secs / BigDecimal(factor * 31557600)).setScale(2, RoundingMode.HALF_UP).toDouble

}