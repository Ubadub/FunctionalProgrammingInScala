package chapter05

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  /**
    * Exercise 5.1: Write a function to convert a Stream to a List, forcing its evaluation.
    */
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] =  {
      s match {
        case Empty => acc
        case Cons(h,t) => go(t(), h() :: acc)
      }
    }

    go(this, Nil).reverse
  }

  /**
    * Exercise 5.2: Write a function that returns the first n elements of a Stream.
    */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h,t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  /**
    * Exercise 5.3: Write a function that skips the first n elements of a Stream (final keyword to ensure it remains
    * tail recursive and cannot be overridden).
    */
  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t() drop n - 1
      case _ => this
    }
  }

  /**
    * Exercise 5.3: Write the function takeWhile for returning all starting elements matching the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h: A,t) if p(h) => cons(h(), t() takeWhile p)
      case _ => empty
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
