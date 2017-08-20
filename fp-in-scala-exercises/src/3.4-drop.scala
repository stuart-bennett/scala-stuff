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

  def tail[A](a: List[A]): List[A] = a match {
    case Nil => sys.error("Attempting to take the tail of an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => sys.error("No head value to replace as list is empty")
    case Cons(_, t) => Cons(a, t)
  }

  // Exercise 3.4 Remove first n elements from a List. ** MY IMPLEMENTATION **
  // Where I went wrong
  //  - Didn't realise could do if ... else {pattern match here}
  //  - Mine throws exception (via tail). Book says this isn't conventional in drop operations
  //  - Overcomplicated it? Didn't need internal loop function - perhaps just thinking about how I did the fib exercise
  def drop_mine[A](l: List[A], n: Int): List[A] = {
    def loop(n: Int, acc: List[A]): List[A] = n match {
      case 0 => acc
      case _ => loop(n-1, List.tail(acc))
    }

    loop(n, l)
  }

  // Exercise 3.4 Remove first n elements from a List. ** ACTUAL SOLUTION **
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
