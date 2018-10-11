package chapter05

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
    * Exercise 5.1: Write a function to convert a Stream to a List, forcing its evaluation.
    *
    * (This implementation could be made more efficient by using local mutable variables, thereby avoiding the reverse
    * at the end.)
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
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
    * Exercise 5.4: Implement forAll, which checks that all elements in the Stream match a given predicate. Your
    * implementation should terminate the traversal as soon as it encounters a non-matching value.
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
    * Exercise 5.5: Use foldRight to implement takeWhile
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, acc) =>
      if (p(h)) cons(h, acc): Stream[A]
      else empty[A])
  }

  /**
    * Exercise 5.6: Implement headOption using foldRight
    */
  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  /**
    * Exercise 5.7: Implement map using foldRight.
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  /**
    * Exercise 5.7 (cont'd): Implement filter using foldRight.
    */
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  /**
    * Exercise 5.7 (cont'd): Implement append using foldRight.
    */
  def append[B>:A](r: => Stream[B]): Stream[B] = foldRight(r)((h, t) => cons(h, t))

  /**
    * Exercise 5.7 (cont'd): Implement flatMap using foldRight.
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  /**
    * Exercise 5.13: Use unfold to implement map
    */
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  /**
    * Exercise 5.13 (cont'd): Use unfold to implement take
    */
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i-1)))
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case _ => None
  }

  /**
    * Exercise 5.13 (cont'd): Use unfold to implement takeWhile
    *
    */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  /**
    * Exercise 5.13 (cont'd): Use unfold to implement zipWith
    */
  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  /**
    * Exercise 5.13 (cont'd): Use unfold to implement zipAll, which continues the traversal as long as either stream has
    * more elements- it uses Option to indicate whether each stream has been exhausted.
    */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), e) => Some(((Some(h1()), None), (t1(), e)))
    case (e, Cons(h2, t2)) => Some(((None, Some(h2())), (e, t2())))
    case _ => None
  }

  @tailrec
  final def startsWith_1[A](s2: Stream[A]): Boolean = (this, s2) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) => h1() == h2() && (t1() startsWith_1 t2())
    case _ => false
  }

  /**
    * Exercise 5.14: Implement startsWith using the functions you've written.
    */
  def startsWith[A](s2: Stream[A]): Boolean = zipAll(s2).takeWhile(_._2.isDefined) forAll {
    case (h1, h2) => h1 == h2
  }

  /**
    * Exercise 5.15: Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input
    * sequence, starting with the original Stream. For example, given Stream(1,2,3), it would return
    * Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
    */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s.drop(1)))
  } append Stream(empty)

  /**
    * Exercise 5.16: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the
    * intermediate results. Your function should reuse intermediate results so that traversing a Stream with n elements
    * always takes time linear in n.
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  /*
  // TODO: Check if this implementation is as good as the one above
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z))((a,b) => (a, b) match {
    case (a, Cons(h, t)) => {
      lazy val v = f(a, h())
      cons(v, Cons(h, t))
    }
    case _ => Stream(z)
  })
  */

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

  val ones: Stream[Int] = cons(1, ones)

  /**
    * Exercise 5.8: Generalize ones slightly to the function constant, which returns an infinite Stream of a given value
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Another way: more efficient b/c tail gets cached (b/c of lazy keyword)
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
    * Exercise 5.9: Write a function that generates an infinite stream of the integers, starting from n, then n+1, etc.
    */
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  /**
    * Exercise 5.10: Write a function fibs the infinite stream of the Fibonacci numbers (0, 1, 1, 2, 3, 5, 8, etc.)
    */
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  /**
    * Exercise 5.11: Write a more general stream-building function called unfold. It takes an initial state and a
    * function for producing both the next state and the next value in the generated stream.
    * @param z the initial state
    *
    * @param f the function for producing the next state and the next value
    *
    * @tparam A the type of each value
    *
    * @tparam S the type of each states
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  /**
    * Exercise 5.12: Write constant in terms of unfold.
    */
  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a,a)))
  }

  /**
    * Exercise 5.12 (cont'd): Write ones in terms of unfold.
    */
  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)

  /**
    * Exercise 5.12 (cont'd): Write from in terms of unfold.
    */
  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s,s+1)))
  }

  /**
    * Exercise 5.12 (cont'd): Write fibs in terms of unfold.
    */
  val fibsViaUnfold: Stream[Int] = {
    unfold((0, 1))({
      case (f0, f1) => Some(f0, (f1, f0+f1))
    })
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}