// "Hello world"
//{
//  println("here")
//  1;
//  println("now here")
//  2;
//  3
//}

import poker.Nill.forAll
import poker.::._
import chapter_2._
import poker.Nill
import poker._
import poker.Nill.distinct
import poker.Nill.map
import poker.Card

case class Person(name: String, lastname: String)
Person("s", "last")
Person("aaa", "d").name
Person("name", "last") == Person("name", "last")
val person = Person("q", "w")
person.copy()


case class Counter(count: Int){
  def inc = copy(count + 1)
  def dec = copy(count - 1)
}

Counter(0).inc.inc

val list = 1::2::3::4::Nill
val l2 = Nill.toString
val l3 = 1::2::3::3::3::4::Nill

distinct(l3)
list.find(_==3)
val opt = Some(3)
opt.filter(_==4)
val opt2:Option[String] = poker.None
opt2.orElse(Some(4))

val isGreaterThanOne = list.forAll(_ > 0)

list.map(_ * 2)

val ll = 2::Nill
val lll = ll.map(_*5)
ll.hashCode()
lll.hashCode()

val cards = Card(Ten, Spades) :: Card(Queen, Spades) :: Card(Two, Hearts) :: Nill
poker.Hand.ranks2(cards)

val list1 = 1::2::3::Nill
val list2 = 4::5::6::Nill
list2:::list1


