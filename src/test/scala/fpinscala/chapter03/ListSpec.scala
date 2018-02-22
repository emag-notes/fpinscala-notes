package fpinscala.chapter03

import fpinscala.BaseSpec
import fpinscala.chapter03.List._

class ListSpec extends BaseSpec {

  "The pattern match" should "match `Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y`" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }

    assert(x === 3)
  }

  "tail" should "return all elements except the first" in {
    assert(tail(List(1, 2, 3)) === List(2, 3))
  }

  "setHead" should "return a list replaced the first" in {
    assert(setHead(42, List(1, 2, 3)) === List(42, 2, 3))
  }

  "drop" should "return all elements except first n ones" in {
    assert(drop(List(1, 2, 3), 2) === List(3))
  }

  "dropWhile" should "return all elements except first n ones" in {
    assert(dropWhile(List(1, 2, 3), (x: Int) => x % 2 != 0) === List(2, 3))
  }

  "append" should "return appended List" in {
    assert(append(List(1, 2, 3), List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  }

  "init" should "return all elements except the last" in {
    assert(append(List(1, 2, 3), List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  }

  "dropWhileCurried" should "return all elements except first n ones" in {
    assert(dropWhileCurried(List(1, 2, 3))(_ % 2 != 0) === List(2, 3))
  }

  "sum2" should "return the sum of elements" in {
    assert(sum2(List(1, 2, 3)) === 6)
  }

  "product2" should "return the product of elements" in {
    assert(sum2(List(1, 2, 3)) === 6)
  }

  "length" should "return the length of elements" in {
    assert(List.length(List('a, 'b', 'c)) === 3)
  }

  "sum3" should "return the sum of elements" in {
    assert(sum3(List(1, 2, 3)) === 6)
  }

  "product3" should "return the product of elements" in {
    assert(sum3(List(1, 2, 3)) === 6)
  }

  "reverse" should "return the reversed list" in {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  "append2" should "return appended List" in {
    assert(append2(List(1, 2, 3), List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  }

  "add1" should "return added1 list" in {
    assert(add1(List(1, 2, 3)) === List(2, 3, 4))
  }

  "toString" should "return stringified list" in {
    assert(
      doubleToString(List(1.0D, 2.0D, 3.0D)) ===
        List("1.0", "2.0", "3.0")
    )
  }

  "map" should "return a list applied given function" in {
    assert(map(List(1, 2, 3))(e => e * 2) === List(2, 4, 6))
  }

  "filter" should "return a filtered list" in {
    assert(filter(List(1, 2, 3))(e => e % 2 == 0) === List(2))
  }

  "flatMap" should "return a flatten list" in {
    assert(flatMap(List(1, 2, 3))(e => List(e, e)) === List(1, 1, 2, 2, 3, 3))
  }

  "filterByFlatMap" should "return a filtered list" in {
    assert(filterByFlatMap(List(1, 2, 3))(e => e % 2 == 0) === List(2))
  }

  "cons" should "return a cons list" in {
    assert(cons(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
  }

  "zipWith" should "return a zipped list" in {
    assert(
      zipWith(List(1, 2, 3), List(4.0, 5.0, 6.0))((a, b) => s"*${a * b}*") ===
        List("*4.0*", "*10.0*", "*18.0*")
    )
  }

  "startsWith" should "check weather or not the elements start with given prefix" in {
    assert(startsWith(List(1, 2, 3, 4), List(1, 2)))
  }

  "hasSubSequence" should "check weather or not the sub list contains in given list" in {
    assert(hasSubSequence(List(1, 2, 3, 4), List(2, 3)))
  }

}
