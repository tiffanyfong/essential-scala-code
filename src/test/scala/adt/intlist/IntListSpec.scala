package adt.intlist

import org.scalatest._

class IntListSpec extends FlatSpec with Matchers {
  val ints = IntPair(1, IntPair(2, IntPair(3, IntNil)))

  "intList.contains" should "return true and false appropriately" in {
    ints.contains(1) should equal(true)
    ints.contains(5) should equal(false)
  }

  "intList.add" should "increment every element in the list" in {
    val expected1 = IntPair(2, IntPair(3, IntPair(4, IntNil)))
    val expected5 = IntPair(6, IntPair(7, IntPair(8, IntNil)))
    ints.add(1) should equal(expected1)
    ints.add(5) should equal(expected5)
  }

  "intList.multiply" should "multiply every element in the list" in {
    val expected0 = IntPair(0, IntPair(0, IntPair(0, IntNil)))
    val expected2 = IntPair(2, IntPair(4, IntPair(6, IntNil)))

    ints.multiply(0) should equal(expected0)
    ints.multiply(2) should equal(expected2)
  }

  "intList.prepend" should "prepend to the head of the list" in {
    ints.prepend(0) should equal(IntPair(0, ints))
  }

  "intList.append" should "append to the tail of the list" in {
    val expected = IntPair(1, IntPair(2, IntPair(3, IntPair(4, IntNil))))
    ints.append(4) should equal(expected)
  }

  "intList.total" should "total all elements" in {
    ints.total should equal(1 + 2 + 3)
  }
}
