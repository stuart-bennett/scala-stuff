package fpinscala.datastructures

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

  // Exercise 3.2
  def tail[A](a: List[A]): List[A] = a match {
    // Throwing an exception is one strategy...
    // Could have returned parameter a, Nil or List() but this could be considered a bug to "allow" a tail of an empty list
    // This will prob be resolved with Option later I imagine
    case Nil => sys.error("Attempting to take the tail of an empty list")
    case Cons(_, t) => t
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


