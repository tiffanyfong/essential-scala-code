package adt.json

sealed trait JsValueList {
  def stringify: String = this match {
    case JsValueListNil => ""
    case JsValuePair(h, t) => h.stringify + "," + t.stringify
  }
}
case object JsValueListNil extends JsValueList
final case class JsValuePair(h: JsValue, t: JsValueList) extends JsValueList

sealed trait JsFieldList {
  def stringify: String = this match {
    case JsFieldListNil => ""
    case JsFieldPair(k, v, t) =>
      "\"" + k + "\":" + v.stringify + "," + t.stringify
  }
}
case object JsFieldListNil extends JsFieldList
final case class JsFieldPair(key: String, value: JsValue, t: JsFieldList) extends JsFieldList

sealed trait JsValue {
  def stringify: String = this match {
    case JsBool(b) => b.toString
    case JsNumber(d) => d.toString
    case JsString(s) => "\"" + s + "\""
    case JsNull => "\"null\""
    case JsArray(vl) => "[" + vl.stringify + "]"
    case JsObject(fl) => "{" + fl.stringify + "}"
  }
}
case object JsNull extends JsValue
final case class JsBool(value: Boolean) extends JsValue
final case class JsNumber(value: Double) extends JsValue
final case class JsString(value: String) extends JsValue
final case class JsArray(value: JsValueList) extends JsValue
final case class JsObject(value: JsFieldList) extends JsValue


object Main extends App {
  // TODO: Create and stringify some JsValues
  val json = JsObject(
    JsFieldPair("a", JsNumber(1),
      JsFieldPair("b", JsString("c"),
        JsFieldPair("c",
          JsArray(
            JsValuePair(JsBool(false),
              JsValuePair(JsNull,
                JsValueListNil))),
          JsFieldListNil))))

  println(json.stringify)


}
