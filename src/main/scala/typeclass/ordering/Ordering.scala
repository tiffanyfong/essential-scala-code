package typeclass.ordering

final case class Email(address: String)
final case class Person(name: String, email: Email)

object OrderingImplicits {
  import math.Ordering

  // STANDARD - implicit
  implicit val emailOrdering: Ordering[Email] =
    new Ordering[Email] {
      def compare(x: Email, y: Email): Int =
        if (x.address < y.address) -1
        else if (x.address > y.address) 1
        else 0
    }

//  val personOrdering: Ordering[Person] =
//    new Ordering[Person] {
//      def compare(x: Person, y: Person): Int =
//        if (x.name < y.name) -1
//        else if (x.name > y.name) 1
//        else emailOrdering.compare(x.email, y.email)
//    }

  implicit val personOrdering: Ordering[Person] =
    Ordering.by[Person, (String, Email)] {
      case Person(name, email) => (name, email)
    }

  // order by email, then name if emails are the same
  val personOrderingByEmail: Ordering[Person] =
    Ordering.by[Person, (Email, String)] {
      case Person(name, email) => (email, name)
    }

//  val personOrderingByEmail: Ordering[Person] = Ordering.by(_.email)

//  implicit val personOrderingByEmail: Ordering[Person] =
//    new Ordering[Person] {
//      def compare(x: Person, y: Person): Int =
//        emailOrdering.compare(x.email, y.email) match {
//          case 0 =>
//            if (x.name < y.name) -1
//            else if (x.name > y.name) 1
//            else 0
//          case s => s
//        }
//    }
}

object Main extends App {
  import OrderingImplicits._

 // implicit val po: Ordering[Person] = personOrdering
  val email1 = Email("alice@cool.com")
  val email2 = Email("charlie@excellent.com")
  val email3 = Email("bob@awesome.com")
  val emails = List(email1, email2, email3)

  val person1 = Person("hey",   Email("alice@wow.com"))
  val person2 = Person("Alice", Email("alice@wow.com"))
  val person3 = Person("Bob",     Email("bob@awesome.com"))
  val people = List(person1, person2, person3)

  println("""emailOrdering.compare(email1, email2)    == """ + emailOrdering.compare(email1, email2))
  println("""personOrdering.compare(person1, person2) == """ + personOrdering.compare(person1, person2))

  println("""emails.sorted == """ + emails.sorted)
  println("""people.sorted == """ + people.sorted(personOrderingByEmail))
}
