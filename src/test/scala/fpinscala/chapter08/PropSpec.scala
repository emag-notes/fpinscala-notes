package fpinscala.chapter08

import java.util.concurrent.Executors

import Prop._
import Gen._
import fpinscala.BaseSpec
import fpinscala.chapter07.Par

class PropSpec extends BaseSpec {

  val smallInt = Gen.choose(-10, 10)
  val ES       = Executors.newCachedThreadPool()

  "max value" should "be the most biggest value" in {
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  "sorted" should "be an ordered list" in {
    val sortedProp = forAll(listOf(smallInt)) { l =>
      val ls = l.sorted
      l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
    }
    Prop.run(sortedProp)
  }

  "parallel prop" should "be able to calc" in {
    val p = Prop.forAll(Gen.unit(Par.unit(1)))(i => Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
    Prop.run(p)
  }

  "[using check] parallel prop" should "be able to calc" in {
    val p = check {
      val p  = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
    Prop.run(p)
  }

  "[using check and equal] parallel prop" should "be able to calc" in {
    val p = check {
      Prop
        .equal(
          Par.map(Par.unit(1))(_ + 1),
          Par.unit(2)
        )(ES)
        .get
    }
    Prop.run(p)
  }

//  "fork" should "be able to fork" in {
//    val pint = choose(-100, 100)
//      .listOfN(choose(0, 20))
//      .map(
//        l =>
//          l.foldLeft(Par.unit(0))(
//            (p, i) =>
//              Par.fork {
//                Par.map2(p, Par.unit(i))(_ + _)
//            }
//        )
//      )
//    val forkP = Prop.forAllPar(pint)(i => Prop.equal(Par.fork(i), i)) tag "fork"
//  }
}
