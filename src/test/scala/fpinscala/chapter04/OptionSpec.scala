package fpinscala.chapter04

import fpinscala.BaseSpec

class OptionSpec extends BaseSpec {

  behavior of "mean"

  it should "return a mean value wrapped Some for given seq" in {
    assert(Option.mean(Seq(1, 2, 3)) === Some(2.0D))
  }

  it should "return None when given empty seq" in {
    assert(Option.mean(Seq()) === None)
  }

  case class Employee(name: String, department: String)
  val defaultDepartment = "Default Dept."
  val salesDepartment   = "Sales"
  val joe               = Employee("Joe", "Sales")
  val employees         = Map("Joe" -> joe)

  object Employee {
    def lookupByName(name: String): Option[Employee] = employees.get(name) match {
      case scala.Some(_) => Some(joe)
      case scala.None    => None
    }
  }

  "lookupByName" should "return Option[Employee]" in {
    assert(Employee.lookupByName("Joe").map(_.department) === Some(salesDepartment))
    assert(Employee.lookupByName("Unknown").getOrElse(defaultDepartment) === defaultDepartment)
    assert(
      Employee
        .lookupByName("Joe")
        .map(_.department)
        .filter(_ != "Accounting")
        .getOrElse(defaultDepartment) === salesDepartment
    )
  }

  "lift" should "lift functions to Option context" in {
    val absO: Option[Double] => Option[Double] = Option.lift(math.abs)
    assert(absO(Some(-2)) === Some(2))
  }

  "sequence" should "change List[Option] to Option[List]" in {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
    assert(Option.sequence(Nil) === Some(Nil))
    assert(Option.sequence(List(Some(1), None, Some(3))) === None)
  }

  "traverse" should "return Option[List] from List" in {
    assert(Option.parseInts(List("1", "2", "3")) === Some(List(1, 2, 3)))
    assert(Option.parseInts(List("1", "TWO", "3")) === None)
  }
}
