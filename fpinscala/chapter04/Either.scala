package chapter04

/**
  * Exercise 4.6: Implement Either with the interface given in the text.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B]
  def flatmap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {a <- this; bb <- b} yield f(a, bb)
}

/**
  * Exercise 4.6 (cont'd): Implement Either with the interface given in the text.
  */
case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B) = Left(value)
  override def flatmap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = Left(value)
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]) = b
}

/**
  * Exercise 4.6 (cont'd): Implement Either with the interface given in the text.
  */
case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: A => B) = Right(f(value))
  def flatmap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = Right(value)
}

object EitherFunctions {

  /**
    * Exercise 4.7: Convert a List of Eithers into one Either containing a list of all Right values in the original
    * List if the List has no Lefts; if Left appears even once, the function should return the first Left encountered.
    */
  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = traverse(es)(x => x)

  /**
    * Exercise 4.7 (cont'd): Write a function that sequences the results of a map over a List, but efficiently, i.e.
    * without looking at the list twice (so without using sequence and map). In other words, take a List and apply a
    * given function to it; if the function fails for any of the elements of the list, return a Left of the error,
    * but if it works for all elements of the list, return a Right containing the output List.
    */
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
  }
}