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

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 3.13 Fold Left using Fold Right
  // This one took me ages. Had to write down the program trace a lot of times.
  // The 'accumlator' is a function so that we can build up a series of functions that eventually ends up like:
  // (for as: List(1,2,3))
  // = 1 :: (2 :: (3 :: []))
  // = A :: (b => 2 :: (b => 3 :: b => b))
  // Passing the (z) in the final parenthesis is the invocation that "kicks" off the chain
  // (z = List()) = [] => 1 :: ([] => 2 :: ([] => 3 :: ([] => []))
  // (z = List()) = [] => 1 :: (2 :: (3 :: []))
  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    foldRight(as, (b:B) => b)((a,acc) => b => acc(f(b,a)))(z)

  // Exercise 3,13 Fold Right using Fold Left
  // As for doing FoldLeft with FoldRight, but the function we use to fold switches the left fold (B,A) order of params to the FoldRight order (A,B)
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b:B) => b)((acc, a) => b => acc(f(a,b)))(z)

  def sum3(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product3(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 1)((acc,_) => acc + 1)

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

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List():List[A])((acc, x) => Cons(x, acc))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}



