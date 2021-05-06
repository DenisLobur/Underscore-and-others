package chapter_5

import munit.FunSuite

class ExpressionTest extends FunSuite {
  test("Add two Numbers") {
    val obtained = Addition(Number(1.0), Number(2.0)).eval
    val expected = Success(3.0)
    assertEquals(obtained, expected)
  }

  test("Subtract two Numbers") {
    val obtained = Subtraction(Number(1.0), Number(2.0)).eval
    val expected = Success(-1.0)
    assertEquals(obtained, expected)
  }

  test("Divide two positive Numbers") {
    val obtained = Division(Number(5.0), Number(2.0)).eval
    val expected = Success(2.5)
    assertEquals(obtained, expected)
  }

  test("Divide by Zero") {
    val obtained = Division(Number(5.0), Number(0.0)).eval
    val expected = Failure("Division by zero")
    assertEquals(obtained, expected)
  }

  test("Square root of positive number") {
    val obtained = SquareRoot(Number(4.0)).eval
    val expected = Success(2.0)
    assertEquals(obtained, expected)
  }

  test("Square root of negative number") {
    val obtained = SquareRoot(Number(-4.0)).eval
    val expected = Failure("Square root of negative number")
    assertEquals(obtained, expected)
  }

}
