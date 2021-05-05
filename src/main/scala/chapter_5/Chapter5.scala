package chapter_5


object Chapter5 extends App {

  val list = Pair(1, Pair(2, Pair(3, Pair(4, End()))))
  println("list: " + list)
  println("length: " + list.length)
  println("sum: " + list.sum)
  println("product: " + list.product)
  println("double: " + list.double)

  val flatMapCheck = list.flatMap(x => Pair(x, Pair(-x, End())))
  println("flatMap check: " + flatMapCheck)

  val maybeList: LinkedList[Maybe[Int]] = Pair(Full(3), Pair(Full(2), Pair(Full(1), End())))
  val maybeEmpty = maybeList.map(maybe => maybe.flatMap[Int] { x => if (x % 2 == 0) Full(x) else Empty() })
  println("maybe empty: " + maybeEmpty)

  val tree: Tree[String] = Node(
    Node(Leaf("To"), Leaf("iterate")),
    Node(
      Node(Leaf("is"), Leaf("human")),
      Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
    )
  )

  println(tree.fold[String]((s, r) => s + " " + r, l => l))

  val pair = MyPair[String, Int]("hi", 2)
  println("pair: " + pair)
  println("pair.one: " + pair.one)
  println("pair.two: " + pair.two)

  val left = Failure[Int](1).value
  println("left: " + left)
  val right = Success[String]("foo").value
  println("right: " + right)
  val sum: Sum[Int, String] = Success("foo")
  println(sum)

  val sumMatch = sum match {
    case Failure(x) => x.toString
    case Success(x) => x
  }
  println(sumMatch)

  val perhaps: Maybe[Int] = Empty[Int]
  val perhaps2: Maybe[Int] = Full(1)

  val checkList = Pair(1, End())
  val checkList2 = Pair(2, End())
  println("compare lists: " + checkList.head.compare()



}
