package chapter04

// Hide standard library members
import scala.{Option => _, Either => _, Some => _, None => _}

/**
  * Exercise 4.1: Implement Option with the interface given in the text.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

/**
  * Exercise 4.1 (cont'd): Implement Option with the interface above.
  */
case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(get))
  def getOrElse[B >: A](default: => B): B = get
}

/**
  * Exercise 4.1 (cont'd): Implement None with the interface above.
  */
case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = this
  def getOrElse[B >: Nothing](default: => B): B = default
}

object OptionFunctions {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2: Calculate variance of a Sequence of Doubles using Option and flatMap.
    */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
    * Exercise 4.3: Write a generic function that combines two Option values using a binary function; if either Option
    * value is None, then the return value is None also.
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a flatMap (x => b map (y => f(x,y)))

  /**
    * Exercise 4.4: Convert a List of Options into one Option containing a list of all Some values in the original
    * List if the List has no Nones; if None appears even once, the function should return None.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((opt, optOfList) => map2(opt, optOfList)(_ :: _))
  // could also use pattern matching with flatMap, map, and recursion

  /**
    * Exercise 4.5: Write a function that sequences the results of a map over a List, but efficiently, i.e. without
    * looking at the list twice (so without using sequence and map). In other words, take a List and apply a given
    * function to it; if the function fails for any of the elements of the list, return None, but if it works for all
    * elements of the list, return a Some containing the output List.
    */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
  // could also be implemented with foldRight, similar to sequence above

  /**
    * Exercise 4.5 (cont'd): Reimplement sequence using traverse.
    */
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}