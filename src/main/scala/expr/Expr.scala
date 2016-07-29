package expr

object Expr {

  // TODO: Implement methods
  def palindrome(str: String): Boolean = {
    // recursion
    // O(n/2) time.
    if (str.length <= 1)
      true
    else if (str.head != str.last)
      false
    else
      palindrome(str.drop(1).dropRight(1))

//    // hahaha built-in function
//    // O(n) time. O(n) space.
//    str.reverse == str
  }

  def greet(name: String): String = {
    s"Hello $name!"
  }

  def factorial(n: Long): Long = {
    // iterative. O(n) time. O(1) space.
    var product: Long = 1L
    var count: Long = 2L
    while (count <= n) {
      product *= count
      count += 1
    }
    product

//    // iterative. O(n) time. O(n) space.
//    (2L to n).toList.product

//    // functional. O(n) time. O(1) space.
//    if (n <= 1)
//      1
//    else
//      n * factorial(n-1)
  }

}

object Main extends App {
  import Expr._

  println("""palindrome("taco") == """ + palindrome("taco"))
  println("""greet("Earthlings") == """ + greet("Earthlings"))
  println("""factorial(10) == """ + factorial(0))
}
