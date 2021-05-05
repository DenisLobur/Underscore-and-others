package rockjvm

object CBNvsCBV extends App {
  def callByValue(x: Long): Unit = {
    println("called by value: " + x)
    println("called by value: " + x)
  }

  def callByName(x: => Long): Unit = {
    println("called by name: " + x)
    println("called by name: " + x)
  }

  callByValue(System.nanoTime())
  callByName(System.nanoTime())

  def infinite(): Int = 1 + infinite()

  def printFirst(x: Int, y: => Int) = println(x)

  //printFirst(infinite(), 34)
  printFirst(34, infinite())
}
