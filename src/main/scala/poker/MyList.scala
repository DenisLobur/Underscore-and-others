package poker

import scala.annotation.tailrec

sealed trait MyList[+A] {

  def length: Int = this match {
    case Nil => 0
    case _ :: xs => 1 + xs.length
  }

  def length2: Int =
    foldLeft(0)((acc, _) => acc + 1)

  def ::[B >: A](a: B): MyList[B] =
    poker.::(a, this)

  def :::[B >: A](prefix: MyList[B]): MyList[B] = {
    if (this.isEmpty) prefix
    else if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ::: this
  }

  def distinct[B >: A](sorted: MyList[B]): MyList[B] = {
    sorted match {
      case x :: y :: tail => if (x.equals(y)) distinct(y :: tail) else x :: distinct(y :: tail)
      case _ => sorted
    }
  }

  def head: A = throw new NoSuchElementException("No head for empty List")

  def tail: MyList[A] = throw new NoSuchElementException("No tail for empty List")

  def headOption: Option[A] =
    this match {
      case Nil => None
      case x :: _ => Some(x)
    }

  def isEmpty: Boolean =
    this match {
      case Nil => true
      case _ => false
    }

  def filter(p: A => Boolean): MyList[A] =
    this match {
      case Nil => Nil
      case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
    }

  def splitAt(i: Int): (MyList[A], MyList[A]) =
    (i, this) match {
      case (0, _) => (Nil, this)
      case (_, Nil) => (Nil, Nil)
      case (i, x :: xs) => val (left, right) = xs.splitAt(i - 1)
        (x :: left, right)
    }

  def zip[B >: A](that: MyList[B]): MyList[(A, B)] =
    (this, that) match {
      case (x :: xs, y :: ys) => (x, y) :: xs.zip(ys)
      case _ => Nil
    }

  def drop(n: Int): MyList[A] =
    if (n <= 0 || isEmpty) this
    else tail.drop(n - 1)

  def find(p: A => Boolean): Option[A] =
    this match {
      case Nil => None
      case x :: xs => if (p(x)) Some(x) else xs.find(p)
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Nil => true
      case x :: xs => p(x) && xs.forAll(p)
    }

  override def toString: String = mkString("List(", ",", ")")

  def mkString(start: String, sep: String, end: String): String = {
    def count(list: MyList[A]): String = list match {
      case Nil => ""
      case head :: Nil => s"$head"
      case head :: tail => s"$head$sep${count(tail)}"
    }

    s"$start${count(this)}$end"
  }

  def map[B](f: A => B): MyList[B] =
    this match {
      case Nil => Nil
      case x :: xs => f(x) :: xs.map(f)
    }

  @tailrec
  final def foldLeft[B](z: B)(op: (B, A) => B): B =
    this match {
      case Nil => z
      case x :: xs => xs.foldLeft(op(z, x))(op)
    }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    reverse.foldLeft(z)((a, b) => op(b, a))

  def reverse: MyList[A] =
    foldLeft(MyList.empty[A])((xs: MyList[A], x) => x :: xs)

  def reduceLeft[B >: A](z: B)(op: (B, A) => B): B =
    foldLeft(z)(op)

  def reduce[B >: A](op: (B, B) => B): B = {
    if (isEmpty) {
      throw new NoSuchElementException("reduce on empty list")
    } else {
      tail.reduceLeft[B](head)(op)
    }
  }

  def sorted[B >: A](implicit ord: (B, B) => Int): MyList[A] = {
    val m = length / 2
    if (m == 0) {
      this
    } else {
      val (left, right) = this.splitAt(m)
      merge(left.sorted, right.sorted)
    }
  }

  def merge[B >: A](left: MyList[B], right: MyList[B])(implicit ord: (B, B) => Int): MyList[B] = {
    (left, right) match {
      case (Nil, _) => right
      case (left, Nil) => left
      case (x :: xs, y :: ys) => if (ord(y, x) > 0) {
        y :: merge(left, ys)
      } else {
        x :: merge(xs, right)
      }
    }
  }
}

object MyList {
  def empty[A]: MyList[A] = Nil

  def apply[A](xs: A*): MyList[A] =
    xs.foldRight(empty[A])((x, ls) => x :: ls)
}

case object Nil extends MyList[Nothing]

case class ::[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

