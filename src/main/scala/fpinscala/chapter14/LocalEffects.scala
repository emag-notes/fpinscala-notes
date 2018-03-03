package fpinscala.chapter14

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {

  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      override protected def run(s: S): (A, S) = (memo, s)
    }
  }

  def runtST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

object STApp extends App {
  val p = new RunnableST[(Int, Int)] {
    override def apply[S]: ST[S, (Int, Int)] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x  <- r1.read
        y  <- r2.read
        _  <- r1.write(y + 1)
        _  <- r2.write(x + 1)
        a  <- r1.read
        b  <- r2.read
      } yield (a, b)
  }

  val r = ST.runtST(p)
}
