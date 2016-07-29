package fn.higherorder

sealed trait IntOption {
  def getOrElse(x: Int): Int = this match {
    case IntSome(i) => i
    case IntNone => x
  }
}
case object IntNone extends IntOption
final case class IntSome(i: Int) extends IntOption

sealed trait IntList {
  def contains(value: Int): Boolean = exists(x => x == value)

  def exists(func: Int => Boolean): Boolean = this match {
    case IntPair(h, t) => func(h) || t.exists(func)
    case IntNil => false
  }

  def filter(func: Int => Boolean): IntList = this match {
    case IntNil => IntNil
    case IntPair(h, t) =>
      if (func(h))
        IntPair(h, t.filter(func))
      else t.filter(func)
  }

  def find(func: Int => Boolean): IntOption = this match {
    case IntNil => IntNone
    case IntPair(h, t) =>
      if (func(h))
        IntSome(h)
      else t.find(func)
  }
}

final case class IntPair(head: Int, tail: IntList) extends IntList

case object IntNil extends IntList

object Main extends App {
  val ints = IntPair(1, IntPair(3, IntPair(5, IntNil)))

  println(ints + """.exists(_ > 0)      == """ + ints.exists(_ > 0))
  println(ints + """.exists(_ < 0)      == """ + ints.exists(_ < 0))
  println(ints + """.exists(_ % 2 == 0) == """ + ints.exists(_ % 2 == 0))
  println(ints + """.exists(_ % 2 == 1) == """ + ints.exists(_ % 2 == 1))

  println(ints.filter(_ <= 3))

  val found = ints.find(_ <= 3)
  println(found.getOrElse(-1))
}
