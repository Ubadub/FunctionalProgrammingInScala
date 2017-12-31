package chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25: Write a function to count the number of nodes (leaves and branches) in a tree
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /**
    * Exercise 3.26: Write a function that finds the maximum element in a Tree[Int]
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /**
    * Exercise 3.27: Write a function that finds the maximum path length from the root to any leaf of a tree
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  /**
    * Exercise 3.28: Write a map function that applies a given function to every element of a Tree and returns the new
    * Tree created as a result.
    */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
    * Exercise 3.29: Write a fold function that abstracts over the similarities between size, maximum, depth, and map.
    */
  def fold[A,B](t: Tree[A])(leafFunc: A => B)(branchFunc: (B,B) => B): B = t match {
    case Leaf(v) => leafFunc(v)
    case Branch(left, right) => branchFunc(fold(left)(leafFunc)(branchFunc), fold(right)(leafFunc)(branchFunc))
  }

  // Exercise 3.29 continued: rewrite size, maximum, depth, and map using fold.

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 0)((b1, b2) => 1 + (b1 max b2))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
}