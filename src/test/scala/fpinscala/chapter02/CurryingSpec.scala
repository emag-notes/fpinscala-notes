package fpinscala.chapter02

import fpinscala.BaseSpec
import fpinscala.chapter02.Currying._

class CurryingSpec extends BaseSpec {

  val repeat: (String, Int) => String = (message: String, repeat: Int) => message * repeat

  behavior of "currying"

  it should "currying with a fun" in {
    assert(curry(repeat)("a")(3) === "aaa")
  }

  it should "uncurrying with a fun" in {
    val curriedRepeat = curry(repeat)
    assert(uncurry(curriedRepeat)("a", 3) === "aaa")
  }

}
