package fpinscala.chapter02

import org.scalatest._
import ArraySorter._

class ArraySorterSpec extends FlatSpec with Matchers with DiagrammedAssertions {

  behavior of "isSorted"

  it should "1 is sorted" in {
    assert(isSorted(Array(1), (x: Int, y: Int) => x > y) === true)
  }

  it should "1, 2, 3 is sorted" in {
    assert(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y) === true)
  }

  it should "1, 3, 2 is not sorted" in {
    assert(isSorted(Array(1, 3, 2), (x: Int, y: Int) => x > y) === false)
  }

  it should "a, b, c is sorted" in {
    assert(isSorted(Array("a", "b", "c"), (x: String, y: String) => x > y) === true)
  }

  it should "c, b, a is not sorted" in {
    assert(isSorted(Array("c", "b", "a"), (x: String, y: String) => x > y) === false)
  }
}
