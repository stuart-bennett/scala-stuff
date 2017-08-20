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

  // Exercise 3.8 - Use nil as default and Cons as the function to fold with
  // Relationship between foldRight and Cons?
  //   - Builds up a List[A] the same way that the variadic List(1,2,3...) function does so:
  //     List(1,2,3) == folding right over List(1,2,3) with Cons (You get the same list back!)
  // The book states that one way of thinking about what foldRight "does" is to replace the Nil constructor with the 'z' arg and replace
  // the Cons constructor with the function 'f'. If you provide Nil for z and Cons for f, it does the same thing!
  def nilToFoldRightWithCons() =
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))

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


