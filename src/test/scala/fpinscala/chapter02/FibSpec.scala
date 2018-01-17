package fpinscala.chapter02

import org.scalatest._
import Fib._

class FibSpec extends FlatSpec with Matchers with DiagrammedAssertions {

  behavior of "fib"

  it should "Given 0 is 0" in {
    assert(fib(0) === 0)
  }

  it should "Given 1(0, 1) is 1" in {
    assert(fib(1) === 1)
  }

  it should "Given 2(0, 1, 1) is 1" in {
    assert(fib(2) === 1)
  }

  it should "Given 3(0, 1, 1, 2) is 2" in {
    assert(fib(3) === 2)
  }

  it should "Given 4(0, 1, 1, 2, 3) is 3" in {
    assert(fib(4) === 3)
  }

  it should "Given 5(0, 1, 1, 2, 3, 5) is 5" in {
    assert(fib(5) === 5)
  }

}
