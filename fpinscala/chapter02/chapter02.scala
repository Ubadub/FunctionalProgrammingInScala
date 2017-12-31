package object chapter02 {

  /**
    * Exercise 2.1: Tail recursive function to find nth Fibbonacci number (where the 0th term is 0 & 1st term is 1)
    */
   def fib(n: Int): Int = {
    def go(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else go(n - 1, b, a+b)
    }

    go(n, 0, 1)
  }

  /**
    * Exercise 2.2: Tail rec. func. to check if a polymorphic Array is sorted according to some ordering function.
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else if (!ordered(as(i - 1), as(i))) false
      else loop(i+1)
    }

    loop(1)
  }

  /**
    * Exercise 2.3: Converts the given function of two arguments into a function of one argument that partially applies
    * it
    */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  /**
    * Exercise 2.4: Reverses the transformation of curry
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /**
    * Exercise 2.5: Composes two functions
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(fib(1))
    println(isSorted(Array(0, 0, 1, 4, 7, 10), (x: Int, y: Int) => x <= y))
  }
}