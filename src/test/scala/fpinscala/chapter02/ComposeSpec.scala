package fpinscala.chapter02

import org.scalatest._
import Compose._

class ComposeSpec extends FlatSpec with Matchers with DiagrammedAssertions {

  val square: Double => Double = (x: Double) => x * x
  val show: Double => String   = (x: Double) => x.toString

  behavior of "compose"

  it should "compose two functions" in {
    val squareAndShow = compose(show, square)
    assert(squareAndShow(3D) === "9.0")
  }

}
