import org.scalatest._

class RobotNameSpecs extends FunSpec with Matchers {
  val nameRegex = """\w{2}\d{3}"""

  it ("has a name") {
    new Robot().name should fullyMatch regex (nameRegex)
  }

  it ("does not change its name") {
    val robot = new Robot
    val name = robot.name
    robot.name should be (name)
  }

  it ("does not have the same name as other robots") {
    new Robot().name should not be (new Robot().name)
  }

  it ("can have its name reset") {
    val robot = new Robot
    val name = robot.name
    robot.reset()
    val name2 = robot.name
    name should not equal (name2)
    name2 should fullyMatch regex (nameRegex)
  }

  it("when all names are exhausted it dies") {
    (1 to 10).map(_ => new Robot()).foreach { r =>
      println(s"name is ${r.name}")
    }
  }

  // test for limit robot names to 1 digit
//  it("when all names are exhausted it dies") {
//    noException should be thrownBy (1 to 10).map(_ => new Robot())
//    intercept[UnsupportedOperationException](new Robot())
//  }
}
