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

  // Exercise 3.3 Replace the first value of a List with a different value
  // Could also just do: Cons(a, List.tail(x))
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => sys.error("No head value to replace as list is empty")
    case Cons(_, t) => Cons(a, t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


