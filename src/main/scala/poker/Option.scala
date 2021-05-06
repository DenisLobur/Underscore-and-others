package poker

sealed trait Option[+A] {

  def get(): A =
    this match {
      case None => throw new NoSuchElementException("None.get")
      case Some(x) => x
    }

  def isEmpty: Boolean =
    this match {
      case Some(_) => false
      case _ => true
    }

  def filter(p: A => Boolean): Option[A] =
    this match {
      case Some(x) if p(x) => Some(x)
      case _ => None
    }

  def orElse[B >: A](alt: => Option[B]): Option[B] =
    this match {
      case Some(_) => this
      case _ => alt
    }

  def getOrElse[B >: A](alt: => B): B = {
    if (isEmpty) alt
    else
      this.get()

  }

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
}

object Option {
  def apply[A](element: A): Option[A] = {
    if (element == null) None
    else Some(element)
  }
}

case object None extends Option[Nothing]

case class Some[+A](a: A) extends Option[A]
