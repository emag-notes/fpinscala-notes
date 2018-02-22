package fpinscala.chapter02

import fpinscala.BaseSpec
import fpinscala.chapter02.Compose._

class ComposeSpec extends BaseSpec {

  val square: Double => Double = (x: Double) => x * x
  val show: Double => String   = (x: Double) => x.toString

  behavior of "compose"

  it should "compose two functions" in {
    val squareAndShow = compose(show, square)
    assert(squareAndShow(3D) === "9.0")
  }

}
