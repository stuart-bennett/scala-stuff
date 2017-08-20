sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail:List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int  = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  // sum & product are very similar but specify different types
  // Generalise a function by pulling subexpressions out into function arguments, noticing that:
  //   - If a subexpression refers to local vars, subexpression should become a function that accepts those vars as arguments

  // Generlise sum & product
  //  - z: Value to return when @as is Empty.
  //  - f: Function to run when @as is NOT Empty - appends onto the result
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)((a,b) => a + b)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _) // Use underscores to use left-to-right args

  // Exercise 3.7 - Product w/ foldRight short-circuiting on 0.0
  // It's not possible to "short circuit" the fold because arguments are evaluated on each "fold"
  // Problem with big lists is that we have to get to the end of this list before we start "using" the return values of the recursive calls
  // so will unnecessarily traverse to the end given that anything multiplied by 0 will be 0 ideally we'd just stop traversing and return 0
  // The book is more eloquently states: before we ever call the function "f", we evaluate it's argument so for foldRight go all the way to the end of the list,
  // Non-Strict evaluation is needed to support early termination! Not covered yet, Chapter 5 discusses it.
  def productSc(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a,b) => if (a == 0.0) 0 else a * b) // Still traverses the entire list so not possible!

  def tail[A](a: List[A]): List[A] = a match {
    case Nil => sys.error("Attempting to take the tail of an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => sys.error("No head value to replace as list is empty")
    case Cons(_, t) => Cons(a, t) }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  // MODIFIED - Make argument list to allow type inference of f so we can do:
  // dropWhile(List[Int])(i => i < 3)
  // instead of the more verbose
  // dropWhile(List[Int], (i: Int) => i < 3)
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons (_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


