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

  // Exercise 3.10 Tail-recursive generate list-recursion
  // foldRight isn't "stack-safe" because each argument is evaluated prior to function execution and so each return value must be pushed onto
  // the stack before the final result can be "collapsed" or rather, the fold actually evaluated
  // The difference is that in this function foldLeft is in the tail position - because the seed 'z' is applied to the first "pass" we can fully evaluate f 
  // rather than having to get to the end of the list, then evaluating the function "on the way back".
  // foldLeft is (B,A) where as foldRight is (A,B). Due to the promise of applying B "at the end", foldRight has to traverse to the end then evaluate, whereas
  // foldLeft can evaluate immediately
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)((a,b) => a + b)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productSc(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a,b) => if (a == 0.0) 0 else a * b) // Still traverses the entire list so not possible!

  def nilToFoldRightWithCons() =
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))

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



