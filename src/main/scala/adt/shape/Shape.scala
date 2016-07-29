package adt.shape
import math.{Pi, pow}

final case class Color(r: Double, g: Double, b: Double)

// supertype - a shape is either a circle or a rect
// sealed - the only shapes that exists are within this file ONLY
// close the world, so one can extend
// compiler knows the types of shapes
sealed trait Shape {

}

final case class Circle(r: Double, color: Color) extends Shape { // subtype

}
final case class Rect(h: Double, w: Double, color: Color) extends Shape { // subtype

}

object Main extends App {
   val shape1 = Circle(10, Color(1, 0, 0))
   val shape2 = Rect(3, 5, Color(0, 1, 0))

   def area(shape: Shape): Double = shape match {
     case Circle(r, _) => Pi * pow(r,2)
     case Rect(h, w, _) => h * w
   }

   def perimeter(shape: Shape): Double = shape match {
     case Circle(r, _) => 2 * Pi * r
     case Rect(h, w, _) => 2*h + 2*w
   }

   println(shape1 + " " + area(shape1))
   println(shape2 + " " + area(shape2))
  println()
}
