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

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  // Write a function "map" analogous to the method of
  // the same name on "List", that modifies each element
  // in a tree with a given function
  def map_mine[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // OFFICIAL SOLUTION
  // The same. I called the value param match something else but
  // implementation is the same
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
}
