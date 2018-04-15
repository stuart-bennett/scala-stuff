sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // Write a function depth that returns the max path
  // length from the root of a tree to any leaf
  // (Also called height?)
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (1 + depth(l)) max (1 + depth(r))
  }

  // OFFICIAL SOLUTION
  // Almost the same - I (deliberately) included the leaf node
  // in the count but they didn't (due to my miunderstanding of exactly what the depth is)
  // Also they add 1 to the result of the max expression
  // which is neater than what I did by adding 1 to each depth expression and taking the max
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
}
