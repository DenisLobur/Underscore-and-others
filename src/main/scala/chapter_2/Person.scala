package chapter_2

class Person(val firstName: String, val lastName: String) {
  def name: String = s"$firstName $lastName"
}

object Person {
  def apply(name: String): Person = {
    val person = name.split(" ")
    val firstName = person(0)
    val lastName = person(1)

    new Person(firstName, lastName)
  }
}
