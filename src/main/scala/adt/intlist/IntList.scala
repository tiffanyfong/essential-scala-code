package adt.intlist

sealed trait IntList {
  def contains(i: Int): Boolean = this match {
    case IntPair(integer, next) =>
      if (i == integer) true
      else next.contains(i)
    case _ => false
  }

  // Adds i to all elements of the list
  def add(i: Int): IntList = this match {
    case IntPair(integer, next) => IntPair(integer + i, next.add(i))
    case _ => IntNil
  }

  // Multiplies i to all elements of the list
  def multiply(i: Int): IntList = this match {
    case IntPair(integer, next) => IntPair(integer * i, next.multiply(i))
    case _ => IntNil
  }

  // Adds int to the head of the list
  def prepend(i: Int): IntList = this match { // checking nil must come first
    case IntNil => IntPair(i, IntNil)
    case currentList => IntPair(i, currentList)
  }

  // Adds int to the end of the list
  def append(i: Int): IntList = this match {
    case IntPair(integer, next) => IntPair(integer, next.append(i))
    case _ => IntPair(i, IntNil)
  }

  def total: Int = this match {
    case IntPair(i, next) => i + next.total
    case _ => 0
  }

  // Retrieves the last element of the list
  def retrieveTail: Option[Int] = this match {
    case IntNil => None
    case IntPair(i, IntNil) => Some(i)
    case IntPair(_, next) => next.retrieveTail
  }

  def popTail: IntList = this match {
    case IntNil => throw new Exception("Cannot pop from empty list")
    case IntPair(_, IntNil) => IntNil
    case IntPair(i, next) => IntPair(i, next.popTail)
  }
}

final case class IntPair(i: Int, next: IntList) extends IntList
case object IntNil extends IntList

object Main extends App {
  val ints = IntPair(1, IntPair(2, IntPair(3, IntNil)))

  println(ints + """.contains(1) == """ + ints.contains(1))
  println(ints + """.contains(5) == """ + ints.contains(5))

  println(ints + """.add(1) == """ + ints.add(1))
  println(ints + """.add(5) == """ + ints.add(5))

  println(ints + """.multiply(0) == """ + ints.multiply(0))
  println(ints + """.multiply(2) == """ + ints.multiply(2))

  println(ints + """.prepend(0) == """ + ints.prepend(0))
  println(ints + """.append(4) == """ + ints.append(4))

  println(ints + """.total == """ + ints.total)
}
