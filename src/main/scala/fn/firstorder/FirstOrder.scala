package fn.firstorder

object FirstOrder {
  val sumSquares: (Int, Int) => Int =
    (x: Int, y: Int) => x*x + y*y

  val longer: (String, String) => String =
    (str1: String, str2: String) => if (str1.length > str2.length) str1 else str2

  val factorial: Long => Long =
    (n: Long) => if (n <= 1) 1 else n * factorial(n-1)
}

object Main extends App {
  import FirstOrder._

   println("""sumSquares(3, 4) == """ + sumSquares(3, 4))
   println("""longer("fooo", "bar") == """ + longer("fooo", "bar"))
   println("""factorial(5) == """ + factorial(5))
}
