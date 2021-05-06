package poker.std

import scala.language.postfixOps

trait Ordering[-A] {
  def compare(x: A, y: A): Int

  def reverse: Ordering[A] =
    (x, y) => compare(y, x)
}

class OrderingOps[A](x: A)(ordering: Ordering[A]) {
  def >(y: A): Boolean =
    ordering.compare(x, y) > 0

  def <(y: A): Boolean =
    ordering.compare(x, y) < 0

  def equiv(y: A): Boolean =
    ordering.compare(x, y) == 0
}
