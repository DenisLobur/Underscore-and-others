package chapter_2

object Chapter2 {
  def main(args: Array[String]): Unit = {
    assert(square(2.0) == 4.0)
    assert(square(3.0) == 9.0)
    assert(square(-2.0) == 4.0)

    val person = Person("Mic Mack")
    println(person.name)
    println(person.firstName)
    println(person.lastName)

  }

  def square(x: Double): Double =
    x * x

}