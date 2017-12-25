package chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.2: implement tail for "removing" the first element of a list and returning it
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of an empty list")
    case Cons(_, t) => t
  }

  /**
    * Exercise 3.3: Implement a function to "replace" first element of a List with a given value
    */
  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => sys.error("setHead on an empty list")
    case Cons(_, t) => Cons[A](newHead, t)
  }

  /**
    * Exercise 3.4: Generalize tail to drop, which removes first n elements
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /**
    * Exercise 3.5: Removes elements from List prefix as long as they match a predicate
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
}