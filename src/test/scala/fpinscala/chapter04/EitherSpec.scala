package fpinscala.chapter04

import fpinscala.BaseSpec

class EitherSpec extends BaseSpec {

  "mean" should "return mean value of given list" in {
    assert(Either.mean(Seq(1, 2, 3)) === Right(2))
    assert(Either.mean(Nil) === Left("mean of empty list!"))
  }

  "safeDiv" should "return divided value safely" in {
    assert(Either.safeDiv(10, 2) === Right(5))
    assert(Either.safeDiv(10, 0).left.getMessage === "/ by zero")
  }

  "parseInsuranceRateQuote" should "return insurance rate" in {
    assert(Either.parseInsuranceRateQuote("21", "2") === Right(42))
    assert(Either.parseInsuranceRateQuote("21", "TWO").left.getClass === new NumberFormatException().getClass)
  }
}
