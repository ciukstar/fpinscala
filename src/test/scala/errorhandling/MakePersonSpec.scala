package errorhandling

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class MakePersonSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "Either.map2" should "lift Person(Name,Age) to a Person(Either[String, Name], Either[String, Age])" in {

    val name = "Sergiu Starciuc"
    val age = 35
    Person.mkPerson(name, age) should be ( Right(Person(new Name(name), new Age(age))) )
    Person.mkPerson("", age) should be ( Left("Name is empty.") )
    Person.mkPerson(null, age) should be ( Left("Name is empty.") )
    Person.mkPerson(name, -35) should be ( Left("Age is out of range.") )
    Person.mkPerson("", -32) should be ( Left("Name is empty.") )
  }
}
