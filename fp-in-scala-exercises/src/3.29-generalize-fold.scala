sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def size_fold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((b1, b2) => 1 + b1 + b2)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def maximum_fold(t: Tree[Int]): Int =
    fold(t)(a => a)((b1, b2) => b1 max b2)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def depth_fold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l,r) => 1 + (l max r))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def map_fold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((l,r) => Branch(l, r))

  // Generalize size, maximum, depth and map writing a new
  // function "fold" that abstracts over their similarities.
  def fold[A,B](t: Tree[A])(f: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => b(fold(l)(f)(b), fold(r)(f)(b))
    }

  // OFFICIAL SOLUTION
  // I had to look at the hints which confirmed I'd got the method
  // signature incorrect. Then I had to check the solution before
  // implementing the "Branch" part of the pattern match.
  // Take away:
  //  - LEARN & PRACTICE MORE WITH FOLDING
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // TAKEAWAYS THE FOLD IMPLEMENTATIONS OF OTHER FUNCTIONS
  //  - It was really easy once the signature of fold was in place,
  //    almost like just making the types line up
  //
  //  - REMEMBER THE "_" NOTATION

  // Same except for param names and I didn't use "_" notation
  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  // Same except that I didn't use the "_" notation
  def maximumViaFold(t: Tree[Int]): Int = 
    fold(t)(a => a)(_ max _)

  // Same except the naming of params
  def depthViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  // I annotated types differently by including the
  // type params in the fold call like [A, Tree[B])
  // I didn't think to annotate the function being passed in
  // Again I also didn't use the "_" notation
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = 
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}
