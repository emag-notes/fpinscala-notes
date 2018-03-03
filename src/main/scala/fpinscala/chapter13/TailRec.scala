package fpinscala.chapter13

import fpinscala.chapter07.Par
import fpinscala.chapter07.Par.Par
import fpinscala.chapter13.FreeOfIO.Console.ConsoleIO

import scala.io.StdIn

object TailRecOfIO {
  sealed trait TailRec[A] { self =>
    def map[B](f: A => B): TailRec[B]              = flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
  }

  case class Return[A](a: A)                                    extends TailRec[A]
  case class Suspend[A](resume: () => A)                        extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    override def unit[A](a: => A): TailRec[A]                                  = Return(a)
    override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa flatMap f
  }

  def printLine(s: String): TailRec[Unit] = Suspend(() => Return(println(s)))

  val p = TailRec.forever(printLine("Still going..."))

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }
}

object AsyncOfIO {
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
    def map[B](f: A => B): Async[B]            = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A)                                extends Async[A]
  case class Suspend[A](resume: Par[A])                     extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    override def unit[A](a: => A): Async[A]                             = Return(a)
    override def flatMap[A, B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)  => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible; `step` eliminates these cases")
      }
  }
}

object FreeOfIO {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B]              = flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A)                                  extends Free[F, A]
  case class Suspend[F[_], A](s: F[A])                              extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A]   = Free[Par, A]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] = new Monad[({ type f[a] = Free[F, a] })#f] {
    override def unit[A](a: => A): Free[F, A]                                  = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)  => runTrampoline { f(a) }
        case Suspend(r) => runTrampoline { f(r()) }
        case FlatMap(a0, g) =>
          runTrampoline {
            a0 flatMap { a0 =>
              g(a0) flatMap f
            }
          }
      }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)              => F.unit(a)
    case Suspend(r)             => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _                      => sys.error("Impossible, since `step` eliminates these cases")
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A

    def toReader: ConsoleReader[A]
    def toState: ConsoleState[A]
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(StdIn.readLine)
      catch { case _: Exception => None }

    override def toReader: ConsoleReader[Option[String]] = ConsoleReader { in =>
      Some(in)
    }

    override def toState: ConsoleState[Option[String]] = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit]    = Par.lazyUnit(println(line))
    override def toThunk: () => Unit = () => println(line)

    override def toReader: ConsoleReader[Unit] = ConsoleReader { s =>
      {}
    }

    override def toState: ConsoleState[Unit] = ConsoleState { bufs =>
      ((), bufs.copy(out = bufs.out :+ line))
    }
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]]      = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]
  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](a: Console[A]): () => A = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible; `step` eliminates these cases")
    }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A                           = () => a
    override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A]                         = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }
  }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      override def apply[A](a: F[A]): FreeG[A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline {
      translate(a)(new (Console ~> Function0) {
        override def apply[A](c: Console[A]): () => A = c.toThunk
      })
    }

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B]                    = ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      override def unit[A](a: => A): ConsoleReader[A]                                              = ConsoleReader(_ => a)
      override def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] = ra flatMap f
    }
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    override def apply[A](a: Console[A]): ConsoleReader[A] = a.toReader
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](io)(consoleToReader)

  case class Buffers(in: List[String], out: Vector[String])
  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] = ConsoleState { s =>
      val (a, s1) = run(s)
      (f(a), s1)
    }

    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

  object ConsoleState {
    implicit val monad = new Monad[ConsoleState] {
      override def unit[A](a: => A): ConsoleState[A]                                           = ConsoleState(bufs => (a, bufs))
      override def flatMap[A, B](a: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] = a flatMap f
    }
  }

  val consoleToState = new (Console ~> ConsoleState) {
    override def apply[A](a: Console[A]): ConsoleState[A] = a.toState
  }

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] = runFree[Console, ConsoleState, A](io)(consoleToState)

}
