package fpinscala.chapter03

import fpinscala.BaseSpec

class TreeSpec extends BaseSpec {

  val tree = Branch(left = Leaf(1), right = Branch(left = Leaf(2), right = Leaf(3)))

  "size" should "return branch and leaf size" in {
    assert(Tree.size(tree) === 5)
  }

  "maximum" should "return maximum value" in {
    assert(Tree.maximum(tree) === 3)
  }

  "depth" should "return the shortest path depth to given leaf" in {
    assert(Tree.depth(tree) === 2)
  }

  "map" should "return the tree applied given function" in {
    assert(
      Tree.map(tree)(e => e * 2) ===
        Branch(left = Leaf(2), right = Branch(left = Leaf(4), right = Leaf(6)))
    )
  }

  "fold" should "generalize the above functions" in {
    assert(Tree.fold(tree)(e => 1)(1 + _ + _) === 5)
    assert(Tree.fold(tree)(e => e)(_ max _) === 3)
    assert(Tree.fold(tree)(e => 0)((d1, d2) => 1 + (d1 max d2)) === 2)
  }
}
