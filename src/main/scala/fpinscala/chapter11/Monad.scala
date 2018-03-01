package fpinscala.chapter11

import fpinscala.chapter06.State
import fpinscala.chapter07.Par
import fpinscala.chapter07.Par.Par
import fpinscala.chapter08.Gen

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) => compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A]                          = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A]                          = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A]                                = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A]                                = Stream(a)
    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A]                            = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A]                         = Id(a)
    override def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }

  def stateMonad[S] = new Monad[({ type lambda[x] = State[S, x] })#lambda] {
    override def unit[A](a: => A): State[S, A]                                    = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

  def getState[S]: State[S, S]          = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))(
        (acc, a) =>
          for {
            xs <- acc
            n  <- getState
            _  <- setState(n + 1)
          } yield (n, a) :: xs
      )
      .run(0)
      ._1
      .reverse

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B]         = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}
