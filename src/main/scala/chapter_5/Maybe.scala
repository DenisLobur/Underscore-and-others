package chapter_5

sealed trait Maybe[A] {
  def fold[B](full: A => B, empty: B): B = {
    this match {
      case Empty() => empty
      case Full(value) => full(value)
    }
  }

  def map[B](f: A => B): Maybe[B] =
    this match {
      case Empty() => Empty[B]()
      case Full(value) => Full(f(value))
    }

  def map2[B](f: A => B): Maybe[B] =
    flatMap[B](v => Full(f(v)))

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    this match {
      case Empty() => Empty[B]()
      case Full(value) => f(value)
    }
}

case class Full[A](value: A) extends Maybe[A]

case class Empty[A]() extends Maybe[A]
