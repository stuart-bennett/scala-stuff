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
  
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)((a,b) => a + b)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productSc(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a,b) => if (a == 0.0) 0 else a * b) // Still traverses the entire list so not possible!

  def nilToFoldRightWithCons() =
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))

  // Exercise 3.9 Compute length of list with foldRight
  // The offical solution uses better var names 'acc' instead of 'b' but otherwise the same
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)

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



