package chapter03

import scala.annotation.tailrec

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
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
    * Exercise 3.6: Return a List minus its last element
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of an empty List")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  /**
    * Exercise 3.9: Calculate length of a list using foldRight
    */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_: A, sum: Int) => sum + 1)

  /**
    * Exercise 3.10: Implement foldLeft, which is like foldRight but tail recursive
    */
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * Exercise 3.11 (1): Write a sum function using foldLeft
    */
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  /**
    * Exercise 3.11 (2): Write a product function using foldLeft
    */
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  /**
    * Exercise 3.11 (3): Write a length function using foldLeft
    */
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  /**
    * Exercise 3.12: Write a function that reverse a List using foldLeft
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  /**
    * Exercise 3.13 (1): Can we implement foldRight in terms of foldLeft?
    */
  def foldRightByFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

  /**
    * Exercise 3.13 (2): Can we implement foldLeft in terms of foldRight?
    */
  def foldLeftByFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = ??? // TODO

  /**
    * Exercise 3.14: Implement append in terms of foldRight or foldLeft
    */
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))

  /**
    * Exercise 3.15: Write a function that flattens a list of lists into a single list, with runtime proportional to the
    * total length of all the lists.
    */
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  /**
    * Exercise 3.16: Write a function that adds 1 to every Int in a List of Ints
    */
  def add1(l: List[Int]): List[Int] = foldRight(l, List[Int]())((i,t) => Cons(i + 1, t))

  /**
    * Exercise 3.17: Write a function that turns each Double in a List of Doubles into a String
    */
  def doubleListToStringList(l: List[Double]): List[String] =
    foldRight(l, List[String]())((d, t) => Cons(d.toString, t))

  /**
    * Exercise 3.18 (1): Write a map function to apply a function to every element of a List (while maintaining its
    * structure). Non-stack-safe version.
    */
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
    * Exercise 3.18 (2): Write a map function to apply a function to every element of a List (while maintaining its
    * structure). Stack-safe version.
    */
  def map_1[A,B](as: List[A])(f: A => B): List[B] = foldRightByFoldLeft(as, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
    * Exercise 3.19: Write a function that removes all elements from a list which do not satisfy a given predicate.
    * Stack-safe.
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightByFoldLeft(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  /**
    * Exercise 3.20: Write a flatMap function which is like map but with a function that returns a List, and which
    * then flattens the resulting Lists into one List. Stack-safe.
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map_1(as)(f))

  /**
    * Exercise 3.21: Rewrite filter using flatMap.
    */
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean) = flatMap(as)(a => if (f(a)) List(a) else Nil)

  /**
    * Exercise 3.22: Write a function that returns a new List by adding corresponding elements from two Lists (if the
    * Lists are of different lengths, return Nil)
    */
  def addCorrespondingElems(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addCorrespondingElems(t1, t2))
  }

  /**
    * Exercise 3.22: Generalize the previous function to operate on Lists of any types and any function, not just
    * Integers with addition
    */
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
    * Exercise 3.22: Generalize the previous function to operate on Lists of any types and any function, not just
    * Integers with addition
    */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hsup, tsup), Cons(hsub, tsub)) => (hsup == hsub) && startsWith(tsup, tsub)
    }

    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(hsup, tsup), Cons(hsub, tsub)) => startsWith(sup, sub) || hasSubsequence(tsup, sub)
    }
  }
}