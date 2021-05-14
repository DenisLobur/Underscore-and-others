package chapter_7

final case class Rational(numerator: Int, denominator: Int)

object Chapter7 extends App {
  val absOrdering: Ordering[Int] = Ordering.fromLessThan[Int]((x, y) => Math.abs(x) < Math.abs(y))

  val rationalOrdering: Ordering[Rational] = Ordering.fromLessThan[Rational]((x, y) => {
    val first = Rational(x.numerator * y.denominator, x.denominator * y.denominator)
    val second = Rational(y.numerator * x.denominator, y.denominator * x.denominator)
    first.numerator < second.numerator
  })

  assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
  assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))

  println(s"compare rationals: ${List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted(rationalOrdering)}")
  assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted(rationalOrdering) == List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
}
