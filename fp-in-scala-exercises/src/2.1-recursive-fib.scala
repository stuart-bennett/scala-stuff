object MyModule {
  def fib(n: Int): Int = {
      @annotation.tailrec
      def go(n: Int, prev: Int, acc: Int): Int = n match {
        case 0 => prev
        case _ => go(n-1, acc, prev + acc)
      }

    go(n, 0, 1)
  }
}
