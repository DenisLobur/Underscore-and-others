package chapter_6

object Chapter6 extends App {

  def addOptions(x: Option[Int], y: Option[Int]): Option[Int] = {
    for {
      optionX <- x
      optionY <- y
    } yield optionX + optionY
  }

  def addOptions(optionX: Option[Int], optionY: Option[Int], optionZ: Option[Int]): Option[Int] = {
    for {
      x <- optionX
      y <- optionY
      z <- optionZ
    } yield x + y + z
  }

  def addOptionsMapFlatMap(x: Option[Int], y: Option[Int]): Option[Int] = {
    x flatMap { valueX =>
      y map { valueY =>
        valueX + valueY
      }
    }
  }

  def addOptionsMapFlatMap(optionX: Option[Int], optionY: Option[Int], optionZ: Option[Int]): Option[Int] = {
    optionX flatMap { x =>
      optionY flatMap { y =>
        optionZ map { z =>
          x + y + z
        }
      }
    }
  }

  def divide(x: Int, y: Int): Option[Int] = {
    if (y == 0) {
      None
    } else {
      Some(x / y)
    }
  }

  def divideOptions(optionX: Option[Int], optionY: Option[Int]): Option[Int] = {
     for {
      x <- optionX
      y <- optionY
      res <- divide(x,y)
    } yield res
  }

  println(s"adding two options: ${addOptions(Some(1), Some(5))}")
  println(s"adding two options using map fmap: ${addOptionsMapFlatMap(Some(7), Some(5))}")
  println(s"adding three options: ${addOptions(Some(1), Some(2), Some(3))}")
  println(s"adding thee options using map fmap ${addOptionsMapFlatMap(Some(1), Some(2), Some(3))}")
  println(s"divide ${divide(4, 2)}")
  println(s"divide ${divide(3, 0)}")
  println(s"divideOptions ${divideOptions(Some(1), Some(2))}")
}
