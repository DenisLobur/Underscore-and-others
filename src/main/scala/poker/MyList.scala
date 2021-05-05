package poker

sealed trait MyList[+A] {

  def length: Int = this match {
    case Nill => 0
    case _ :: xs => 1 + xs.length
  }

  def ::[B >: A](a: B): MyList[B] =
    poker.::(a, this)

  def :::[B >: A](prefix: MyList[B]): MyList[B] = {
    if (this.isEmpty) prefix
    else if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ::: this
  }

  def distinct[A](sorted: MyList[A]): MyList[A] = {
    sorted match {
      case x :: y :: tail => if (x.equals(y)) distinct(y :: tail) else x :: distinct(y :: tail)
      case _ => sorted
    }
  }

  def head: A = throw new NoSuchElementException("No head for empty List")

  def tail: MyList[A] = throw new NoSuchElementException("No tail for empty List")

  def headOption: Option[A] =
    this match {
      case Nill => None
      case x :: _ => Some(x)
    }

  def isEmpty: Boolean =
    this match {
      case Nill => true
      case _ => false
    }

  def filter(p: A => Boolean): MyList[A] =
    this match {
      case Nill => Nill
      case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
    }

  def splitAt(i: Int): (MyList[A], MyList[A]) =
    (i, this) match {
      case (0, _) => (Nill, this)
      case (_, Nill) => (Nill, Nill)
      case (i, x :: xs) => val (left, right) = xs.splitAt(i - 1)
        (x :: left, right)
    }

  def zip[B >: A](that: MyList[B]): MyList[(A, B)] =
    (this, that) match {
      case (x :: xs, y :: ys) => (x, y) :: xs.zip(ys)
      case _ => Nill
    }

  def drop(n: Int): MyList[A] =
    if (n <= 0 || isEmpty) this
    else tail.drop(n - 1)

  def find(p: A => Boolean): Option[A] =
    this match {
      case Nill => None
      case x :: xs => if (p(x)) Some(x) else xs.find(p)
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Nill => true
      case x :: xs => p(x) && xs.forAll(p)
    }

  override def toString: String = mkString("List(", ",", ")")

  def mkString(start: String, sep: String, end: String): String = {
    def count(list: MyList[A]): String = list match {
      case Nill => ""
      case head :: Nill => s"$head"
      case head :: tail => s"$head$sep${count(tail)}"
    }

    s"$start${count(this)}$end"
  }

  def map[B](f: A => B): MyList[B] =
    this match {
      case Nill => Nill
      case x :: xs => f(x) :: xs.map(f)
    }
}

case object Nill extends MyList[Nothing]

case class ::[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

