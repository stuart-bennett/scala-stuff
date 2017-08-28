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

  def append[A](as: List[A], a: List[A]): List[A] =
    foldRight(as, a)((a, acc) => Cons(a, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def add1(l: List[Int]) =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString(), t))

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t);
    }

    go(l)
    List(buf.toList: _*) // Converts from standard scala list to our List type
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t);
    }

    go(l);
    return List(buf.toList: _*)
  }

  // Exercise 3.20 Write a function that works like "map" except that the given function will return a list
  // instead of a single result, and that list should be inserted into the final resulting list.
  def flatMap_mine[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, acc) => append(f(a), acc))

  // OFFICIAL SOLUTION
  // What I did differently:
  //   - I duplicated what concat was doing. Had forgotten I'd written concat so should have just used that.
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

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



