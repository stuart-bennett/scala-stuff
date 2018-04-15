sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // Write a function that returns the maximum
  // element in a Tree[Int]
  def maximum_mine(t: Tree[Int]): Int = t match {
    case Leaf(l) => l
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // OFFICIAL SOLUTION
  // Almost identical except their pattern match
  // called the leaf value 'n'
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }
}
