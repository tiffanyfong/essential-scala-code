package adt.color

// final - class cannot be extended
final case class Color(r: Double, g: Double, b: Double) {
  val lightness = (r+g+b)/3

  def lighterThan(c: Color): Boolean = lightness > c.lightness
}

object Main extends App {
  def mostlyRed(c: Color) = c match {
    case Color(r, g, b) =>
      r > g && r > b
  }

  val color1 = Color(1.0, 0.5, 0.0)
  val color2 = Color(0.6, 0.7, 0.8)

  println("color1 " + color1)
  println("color2 " + color2)

  println("Lightness of color1 " + color1.lightness)
  println("Lightness of color2 " + color2.lightness)

  println("Is color1 mostly red? " + mostlyRed(color1))
  println("Is color2 mostly red? " + mostlyRed(color2))
}
