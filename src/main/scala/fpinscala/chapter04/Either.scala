package fpinscala.chapter04

sealed trait Either[+E, +A] {
  def left: E
  def right: A

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Right(a) => Right(a)
      case Left(_)  => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a  <- this
      bb <- b
    } yield f(a, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def left: E = value

  override def right: Nothing = ???
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def left: Nothing = ???

  override def right: A = value
}

object Either {

  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case ex: Exception => Left(ex)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age * numberOfSpeedingTickets).toDouble

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      t <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, t)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil    => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

}
