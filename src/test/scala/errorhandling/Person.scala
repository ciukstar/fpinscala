package errorhandling

case class Person(name: Name, age: Age)
case class Name(val value: String)
case class Age(val value: Int)

object Person {

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age)) (Person(_, _))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

}
