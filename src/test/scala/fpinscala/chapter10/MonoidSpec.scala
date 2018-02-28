package fpinscala.chapter10

import fpinscala.BaseSpec
import Monoid._

class MonoidSpec extends BaseSpec {

  "stringMonoid" should "satisfy Monoid laws" in {
    val words = List("Hic", "Est", "Index")
    assert(
      words.foldRight(stringMonoid.zero)(stringMonoid.op) ===
        words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    )
  }

  "foldMap" should "apply given function" in {
    val numbers = List(1, 2, 3)
    assert(
      foldMap(numbers, stringMonoid)((a) => s"xx $a **") ===
        "xx 1 **xx 2 **xx 3 **"
    )
  }

  "foldMapV" should "apply given function" in {
    val numbers = IndexedSeq(1, 2, 3)
    assert(
      foldMapV(numbers, stringMonoid)((a) => s"xx $a **") ===
        "xx 1 **xx 2 **xx 3 **"
    )
  }

  "count" should "count the number of words" in {
    assert(count("lorem ipsum dolor sit amet, ") === 5)
  }

  "Monoid" should "compose Monoids" in {
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))

    val m3 = M.op(m1, m2)

    assert(m3 === Map("o1" -> Map("i1" -> 1, "i2" -> 5)))
  }

  "bag" should "contains num per word" in {
    assert(
      bag(Vector("a", "rose", "is", "a", "rose")) ===
        Map("a" -> 2, "rose" -> 2, "is" -> 1)
    )
  }

  "Monoid" should "do some calcs at the same time" in {
    val m    = productMonoid(intAddition, intAddition)
    val p    = ListFoldable.foldMap(List(1, 2, 3, 4))((1, _))(m)
    val mean = p._2 / p._1.toDouble
    assert(mean === 2.5)
  }
}
