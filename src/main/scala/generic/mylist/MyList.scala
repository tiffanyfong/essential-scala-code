package generic.mylist

sealed trait MyOption[+T] {
  def map[U](f: T => U): MyOption[U] = this match {
    case MyNone => MyNone
    case MySome(s) => MySome(f(s))
  }

  def flatMap[U](f: T => MyOption[U]): MyOption[U] = this match {
    case MyNone => MyNone
    case MySome(s) => f(s)
  }
}
final case class MySome[T](value: T) extends MyOption[T]
case object MyNone extends MyOption[Nothing]

sealed trait MyList[+T] {
  def map[U](f: T => U): MyList[U] = this match {
    case MyPair(h, t) => MyPair(f(h), t.map(f))
    case MyNil => MyNil
  }

  def contains[U >: T](value: U): Boolean = exists(_ == value)

  def exists(f: T => Boolean): Boolean = this match {
    case MyPair(h, t) => f(h) || t.exists(f)
    case MyNil => false
  }

  def filter(f: T => Boolean): MyList[T] = this match {
    case MyPair(h, t) =>
      if (f(h))
        MyPair(h, t.filter(f))
      else t.filter(f)
    case MyNil => MyNil
  }

  def find(f: T => Boolean): MyOption[T] = this match {
    case MyPair(h, t) =>
      if (f(h))
        MySome(h)
      else t.find(f)
    case MyNil => MyNone
  }

  // append MyList
  def append[U >: T](other: MyList[U]): MyList[U] = this match {
    case MyNil => other
    case MyPair(h, t) => MyPair(h, t.append(other))
  }

  def flatMap[U](f: T => MyList[U]): MyList[U] = this match {
    case MyNil => MyNil
    case MyPair(h, t) => f(h) append t.flatMap(f)
  }

  def foldLeft[U](acc: U)(f: (U,T) => U): U = this match {
    case MyNil => acc
    case MyPair(h, t) => t.foldLeft( f(acc,h) )( f )
  }

  def foldRight[U](acc: U)(f: (T,U) => U): U = this match {
    case MyNil => acc
    case MyPair(h, t) => f( h, t.foldRight(acc)(f) )
  }

  def reverse: MyList[T] = this match {
    case MyNil => MyNil
    case MyPair(h, t) => t.reverse append MyPair(h, MyNil)
  }

  def foldRight2[U](acc: U)(f: (T,U) => U): U =
    reverse.foldLeft(acc)((right, left) => f(left, right))

}
final case class MyPair[+T](value: T, next: MyList[T]) extends MyList[T]
// case objects can't have type parameters. so MyNil must be case class
// variance issue
case object MyNil extends MyList[Nothing]
//final case class MyNil[T]() extends MyList[T]




object Main extends App {
  val l: List[String] = List()
   val ints = MyPair(1, MyPair(3, MyPair(5, MyNil)))
   val strs = MyPair("foo", MyPair("bar", MyPair("baz", MyNil)))

   println(ints + """.exists(_ > 1)         == """ + ints.exists(_ > 1))
   println(ints + """.filter(_ > 1)         == """ + ints.filter(_ > 1))
   println(ints + """.find(_ > 1)           == """ + ints.find(_ > 1))

   println(strs + """.exists(_(0) == 'b')   == """ + strs.exists(_(0) == 'b'))
   println(strs + """.filter(_(0) == 'b')   == """ + strs.filter(_(0) == 'b'))
   println(strs + """.find(_(0) == 'b')     == """ + strs.find(_(0) == 'b'))

   def add(ints: MyList[Int], num: Int): MyList[Int] =
     ints.map(_ + num)

   println("""add(""" + ints + """, 1))     == """ + add(ints, 1))
   println(strs + """.map(_ + "!"))         == """ + strs.map(_ + "!"))

   def processInt(x: Int): MyList[Int] =
     MyPair(2*x, MyPair(x * 10, MyNil))

   def processStr(x: String): MyList[String] =
     MyPair(x, MyPair(x + "!", MyNil))

   println(ints + """.flatMap(processInt)   == """ + ints.flatMap(processInt))
   println(strs + """.flatMap(processStr)   == """ + strs.flatMap(processStr))

   println(ints + """.foldLeft(0)(_ + _))   == """ + ints.foldLeft(0)(_ + _))
   println(ints + """.foldRight(0)(_ + _))  == """ + ints.foldRight(0)(_ + _))

  println("\n\n")
  println(ints.foldLeft("")(_+_))

   println(strs + """.foldLeft("")(_ + _))  == """ + strs.foldLeft("")(_ + _))
   println(strs + """.foldRight("nil")(_ + _)) == """ + strs.foldRight("nil")(_ + _))
}
