package chapter_5

sealed trait LinkedList[+A] {
  def length: Int =
    fold[Int](0) { (_, r) => 1 + r }

  def double: LinkedList[Int] =
    fold[LinkedList[Int]](End[Int]()) { (l, r) => Pair(l * l, r) }

  //  def map2[B](f: A => B): LinkedList[B] =
  //    fold[LinkedList[A]](End[A]()) {(sum, el) => Pair(sum, el)}

  def product: Int =
    fold[Int](1) { (l, r: Int) => l * r }

  def sum: Int =
    fold[Int](0) { (l, r) => l + r }

  def fold[B](end: B)(pair: (Int, B) => B): B =
    this match {
      case End() => end
      case Pair(head, tail) => pair(head.asInstanceOf[Int], tail.fold(end)(pair))
    }

  def map[B](f: A => B): LinkedList[B] =
    this match {
      case End() => End[B]()

      case Pair(x, xs) => Pair(f(x), xs.map(f))
    }

  def flatMap[B](f: A => LinkedList[B]): LinkedList[B] =
    this match {
      case End() => End[B]()
      case Pair(x, xs) => f(x) ::: xs.flatMap(f)
    }

  def ::[B >: A](a: B): LinkedList[B] =
    Pair(a, this)

  def :::[B >: A](prefix: LinkedList[B]): LinkedList[B] =
    prefix match {
      case End() => this
      case Pair(x, xs) => x :: xs ::: this
    }

  override def toString: String =
    mkString("List(", ", ", ")")

  def mkString(start: String, delimiter: String, end: String): String = {
    def compose(list: LinkedList[A]): String = {
      list match {
        case End() => ""
        case Pair(head, End()) => s"$head"
        case Pair(head, tail) => s"$head$delimiter${compose(tail)}"
      }
    }

    s"$start${compose(this)}$end"
  }

  def sorted[B >: A](implicit ord: Ordering[B]): LinkedList[A] =
    ord.compare(_, _)
}

object LinkedList {
  def empty[A]: LinkedList[A] = End[A]()

}

case class End[A]() extends LinkedList[A]

case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
