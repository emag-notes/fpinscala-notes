package fpinscala.chapter06

import fpinscala.BaseSpec
import fpinscala.chapter06.RNG.SimpleRNG

class StateTest extends BaseSpec {

  "RNG" should "return value and state" in {
    val rng1       = SimpleRNG(42)
    val (n1, rng2) = rng1.nextInt
    val (n2, _)    = rng2.nextInt

    assert(rng1.nextInt._1 === n1)
    assert(n1 !== n2)
  }

}
