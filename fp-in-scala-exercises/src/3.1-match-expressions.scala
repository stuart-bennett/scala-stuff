import List._

object Module31 {
  def run = {
    val lst = List(1,2,3,4,5)
    lst match {
      case Cons(x, Cons(2, Cons(4, _))) => x // won't match because missing 3 - ...2, *3*, 4..,
      case Nil => 42 // won't match because lst isn't Nil
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // WILL MATCH because consts (3,4, [anything else]) match lst and we can bind first two elements to x & y
      case Cons(h, t) => h + sum(t) // WOULD HAVE MATCHED because h = 1st el and t will be LIST(2,3,4,5) but appears after another matching pattern
      case _ => 101
    }
  }
}
