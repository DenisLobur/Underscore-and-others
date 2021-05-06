package chapter_5

sealed trait Expression {

  def eval: Sum[String, Double] =
    this match {
      case Addition(left, right) => liftToExpr(left, right, (x, y) => Success(x + y))

      case Subtraction(left, right) => liftToExpr(left, right, (x, y) => Success(x - y))
      case Division(left, right) => liftToExpr(left, right, (x, y) => if (y == 0) Failure("Division by zero") else Success(x / y))
      case SquareRoot(x) => x.eval.flatMap { value =>
        if (value < 0) Failure("Square root of negative number") else Success(Math.sqrt(value))
      }
      case Number(x) => Success(x)
      case _ => Failure("Not implemented yet")
    }

  def liftToExpr(left: Expression, right: Expression, f: (Double, Double) => Sum[String, Double]): Sum[String, Double] = {
    left.eval.flatMap { l =>
      right.eval.flatMap { r =>
        f(l, r)
      }
    }
  }
}

final case class Addition(left: Expression, right: Expression) extends Expression

final case class Subtraction(left: Expression, right: Expression) extends Expression

final case class Division(left: Expression, right: Expression) extends Expression

final case class SquareRoot(value: Expression) extends Expression

final case class Number(value: Double) extends Expression
