package poker

sealed trait Option[+A] {

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

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
}

case object None extends Option[Nothing]

case class Some[+A](a: A) extends Option[A]
