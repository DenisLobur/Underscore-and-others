package poker

object Hand {
  type Cards = MyList[Card]

  trait Hand {

  }

  case object RoyalFlush extends Hand

  case class Straight(high: Rank)

  //TODO fix case List(10♠️, Q♠️, 2♥️, K♠️, A♠️, J♠️, 10♥️)
  def hasRoyalFlush(cs: Cards): Boolean = {
    val fs = filterGte10(cs)
    fs.length == 5 && sameSuit(fs)
  }

  def filterGte10(cs: MyList[Card]): MyList[Card] = {
    cs.filter(c => c.rank >= Ten)
  }

  def sameSuit(cs: Cards): Boolean = {
    cs match {
      case x :: y :: cs =>
        x.suit == y.suit && sameSuit(y :: cs)
      case _ => true
    }
  }

  def straight(cs: Cards) = {
    val rs = distinct(sorted(ranks(cs)))
    rs.zip(rs.drop(4))
      .filter({ case (f: Rank, l: Rank) => f.value == l.value + 4 })
      .headOption
      .map({ case (h, _) => Straight(h) })
    // .orElse(lowStraight(rs))
  }


  def ranks(cs: Cards): MyList[Rank] = {
    cs match {
      case Nil => Nil
      case x :: xs => x.rank :: ranks(xs)
    }
  }

  def ranks2(cs: Cards): MyList[Rank] = cs.map(_.rank)

  def sorted(cs: MyList[Rank]): MyList[Rank] = {
    val m = cs.length / 2
    if (m == 0) cs
    else {
      val (left, right) = cs.splitAt(m)
      merge(sorted(left), sorted(right))
    }
  }

  def merge(left: MyList[Rank], right: MyList[Rank]): MyList[Rank] = {
    (left, right) match {
      case (Nil, right) => right
      case (left, Nil) => left
      case (x :: xs, y :: ys) =>
        if (x.value > y.value) x :: merge(xs, right)
        else y :: merge(left, ys)
    }
  }

  def distinct(sorted: MyList[Rank]): MyList[Rank] = {
    sorted match {
      case x :: y :: xs if (x == y) => distinct(y :: xs)
      case x :: xs => x :: distinct(xs)
      case Nil => Nil
    }
  }
}
