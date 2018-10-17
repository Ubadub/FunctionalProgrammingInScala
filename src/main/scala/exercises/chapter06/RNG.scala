package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // linear congruential generator
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill
      (n, nextRNG) // return pseudo-random integer and the next `RNG` state.
    }
  }

  /**
    * Exercise 6.1: Write a function that uses RNG.nextInt to generate a random integer between 0 and `Int.MaxValue`
    * (inclusive).
    *
    * NB: The answer key on Github uses a similar but not identical approach. Here we map Int.MinValue to 0 and -1 to
    * Int.MaxValue, while the answer key maps Int.MinValue to Int.MaxValue and -1 to 0.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRNG) = rng.nextInt
    if (n < 0)
      (n+Int.MaxValue + 1, newRNG)
    else
      (n, newRNG)
  }

  /**
    * Exercise 6.2: Write a function to generate a `Double` between 0 and 1, not including 1.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, newRNG) = nonNegativeInt(rng)
    (n/(Int.MaxValue.toDouble + 1), newRNG)
  }

  /**
    * Exercise 6.3: Write a function to generate an `(Int, Double)` pair.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  /**
    * Exercise 6.3 (cont'd): Write a function to generate a `(Double, Int)` pair.
    */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  /**
    * Exercise 6.3 (cont'd): Write a function to generate a `(Double, Double, Double)` 3-tuple.
    */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Exercise 6.4: Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count-1)(r1)
      (i :: l, r2)
    }
  }

  /**
    * Represents a state action- a program that depends on some RNG to generate an A, and also transitions the RNG to a
    * new state that can be used by another action later.
    */
  type Rand[+A] = RNG => (A, RNG)

  // equivalent to rng => rng.nextInt
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a,rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /**
    * Exercise 6.5: Use `map` to reimplement `double` in a more elegant way.
    */
  val double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 6.5: Write a function `map2` which takes two actions, `ra` and `rb` and combines their results using a
    * binary function.
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
    * Exercise 6.7: Implement sequence for combining a `List` of transitions into a single transition.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((rand, randAcc) => map2(rand, randAcc)(_ :: _))
  }

  /**
    * Exercise 6.7 (cont'd): Use sequence to reimplement the `ints` function.
    */
  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
    * Exercise 6.8: Implement `flatMap`
    */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  /**
    * Exercise 6.8 (cont'd): Implement `nonNegativeLessThan` using `flatMap`
    */
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i => {
        val mod = i % n
        if (i + (n-1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
      }
    }

  /**
    * Exercise 6.9: Reimplement `map` in terms of flatMap.
    */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  /**
    * Exercise 6.9 (cont'd): Reimplement `map2` in terms of flatMap.
    */
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {

  }

}

import State._

/**
  * Exercise 6.10: Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods on the State
  * case class where possible.
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((s: S) => {
    val (a, rng2) = run(s)
    (f(a), rng2)
  })

  def map2[B,C](s2: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s3) = run(s)
    val (b, s4) = s2.run(s3)
    (f(a, b), s4)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
    val (a, s2) = run(s1)
    f(a).run(s2)
  })

  def mapViaFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2ViaFlatMap[B,C](s2: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => s2.mapViaFlatMap(b => f(a, b)))
}

/**
  * Exercise 6.10 (cont'd): Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods on the
  * State case class where possible.
  */
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}
