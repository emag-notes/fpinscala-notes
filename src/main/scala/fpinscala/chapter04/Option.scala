package fpinscala.chapter04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None    => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None    => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None    => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None    => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _               => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age * numberOfSpeedingTickets).toDouble
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge     = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case _: Exception => None }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil          => Some(Nil)
    case head :: tail => head flatMap (h => sequence(tail) map (h :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil          => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }

  def parseInts(a: List[String]): Option[List[Int]] = traverse(a)(s => Try(s.toInt))

}
