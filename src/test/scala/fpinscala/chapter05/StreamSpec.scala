package fpinscala.chapter05

import fpinscala.BaseSpec

class StreamSpec extends BaseSpec {

  "toList" should "return Stream as List" in {
    assert(Stream(1, 2, 3).take(2).toList === List(1, 2))
  }

  "ones" should "generate infinite stream" in {
    assert(Stream.ones.take(5).toList === List(1, 1, 1, 1, 1))
    assert(Stream.ones.exists(_            % 2 != 0))
    assert(Stream.ones.map(_ + 1).exists(_ % 2 == 0))
  }

  "fibs" should "generate infinite fib stream" in {
    assert(
      Stream.fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
  }

}
