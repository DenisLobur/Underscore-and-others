package chapter_5

class MyPair[A, B](val one: A, val two: B)

object MyPair {
  def apply[A, B](one: A, two: B): MyPair[A, B] = {
    new MyPair[A, B](one, two)
  }
}