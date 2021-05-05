package chapter_5

sealed trait Sum[+A, +B] {
  def fold[C](error: A => C, success: B => C): C = {
    this match {
      case Failure(value) => error(value)
      case Success(value) => success(value)
    }
  }

  def map[C](f: B => C): Sum[A, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(x) => Success(f(x))
    }

  def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(x) => f(x)
    }
}

case class Failure[A](value: A) extends Sum[A, Nothing]

case class Success[B](value: B) extends Sum[Nothing, B]
