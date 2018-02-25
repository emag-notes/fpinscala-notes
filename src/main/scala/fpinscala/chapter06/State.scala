package fpinscala.chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count <= 0) (Nil, rng)
      else {
        val (x, r1)  = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x :: xs, r2)
      }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(_ - 1 % 2)

    def doubleViaMap: Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, _)  = ra(rng)
        val (b, r2) = rb(rng)
        (f(a, b), r2)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, doubleViaMap)

    val randDoubleInt: Rand[(Double, Int)] = both(doubleViaMap, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    def intsViaSequence(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1)
      }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))

    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  }

}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def update: Input => Machine => Machine =
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _))               => s
          case (Coin, Machine(false, _, _))        => s
          case (Turn, Machine(true, _, _))         => s
          case (Coin, Machine(true, candy, coin))  => Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
