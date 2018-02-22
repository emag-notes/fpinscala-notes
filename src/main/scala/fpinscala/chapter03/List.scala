package fpinscala.chapter03

sealed trait List[+A]
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil         => throw new IllegalArgumentException("list must not be empty")
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil         => throw new IllegalArgumentException("list must not be empty")
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil         => throw new IllegalArgumentException("list must not be empty")
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](list: List[A]): List[A] = list match {
    case Nil          => throw new IllegalArgumentException("list must not be empty")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhileCurried(xs)(f)
    case _                   => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())((acc, x) => Cons(x, acc))

  def foldRight2[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(list), z)((b, a) => f(a, b))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((head, tail) => Cons(head + 1, tail))

  def concat[A](list: List[List[A]]): List[A] =
    foldRight(list, Nil: List[A])(append)

  def doubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])((head, tail) => Cons(head.toString, tail))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((head, tail) => Cons(f(head), tail))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])(
      (head, tail) =>
        if (f(head)) Cons(head, tail)
        else tail
    )

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    concat(map(list)(f))

  def filterByFlatMap[A](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list)(e => if (f(e)) List(e) else Nil)

  def cons(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Nil, _)                                 => Nil
    case (_, Nil)                                 => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, cons(tail1, tail2))
  }

  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = (list1, list2) match {
    case (Nil, _)                                 => Nil
    case (_, Nil)                                 => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zipWith(tail1, tail2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, tail)             => hasSubSequence(tail, sub)
  }

}
