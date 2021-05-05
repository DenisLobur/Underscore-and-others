package chapter_5

sealed trait Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B = node(left.fold(node, leaf), right.fold(node, leaf))
}

case class Leaf[A](el: A) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B = leaf(el)
}
