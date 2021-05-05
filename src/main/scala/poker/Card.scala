package poker

sealed trait Suit {

  override def toString: String =
    this match {
      case Clubs => "♣"
      case Diamonds => "♦️"
      case Hearts => "♥️"
      case Spades => "♠️"
    }
}

case object Clubs extends Suit
case object Diamonds extends Suit
case object Hearts extends Suit
case object Spades extends Suit

sealed trait Rank {

  def value(): Int =
    this match {
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case Ten => 10
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
    }

  def >=(rank: Rank): Boolean =
    this.value >= rank.value

  override def toString: String =
    this match {
      case Ace => "A"
      case King => "K"
      case Queen => "Q"
      case Jack => "J"
      case x => x.value.toString
    }
}

case object Two extends Rank
case object Three extends Rank
case object Four extends Rank
case object Five extends Rank
case object Six extends Rank
case object Seven extends Rank
case object Eight extends Rank
case object Nine extends Rank
case object Ten extends Rank
case object Jack extends Rank
case object Queen extends Rank
case object King extends Rank
case object Ace extends Rank

case class Card(rank: Rank, suit: Suit) {
  override def toString: String =
    rank.toString + suit.toString
}
