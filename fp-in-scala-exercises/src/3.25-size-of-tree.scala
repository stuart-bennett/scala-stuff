sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Write a function that counts the number of nodes
  // (leaves and branches) in a tree.
  def size_mine[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // OFFICIAL SOLUTION
  // The same. Much easier than the last exercise
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
}
