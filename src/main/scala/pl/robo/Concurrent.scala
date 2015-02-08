package pl.robo

abstract sealed class Action

case class Atom(atom: Unit => Action) extends Action {
  override def toString = "atom"
}

case class Fork(a1: Action, a2: Action) extends Action {
  override def toString = s"fork ${a1.toString} ${a2.toString}"
}

case class Stop() extends Action {
  override def toString = "stop"
}

class Concurrent[A](val func: (A => Action) => Action) {

  import pl.robo.Concurrent.roundRobin

  def andThen[B](after: Concurrent[B]): Concurrent[B] = flatMap(_ => after)

  def action(): Action = func(_ => Stop())

  def fork(): Concurrent[Unit] = {
    def cont(c: Unit => Action): Action = Fork(action(), c(()))
    Concurrent(cont)
  }

  def flatMap[B](mapper: A => Concurrent[B]): Concurrent[B] = {
    def f1(c : B => Action) : Action = {
      def f2(a:A) : Action = {
        val cb: Concurrent[B] = mapper(a)
        cb.func(c)
      }
      func(f2)
    }

    Concurrent(f1)
  }



  def run(): () => Unit = roundRobin(List[Action](action))
}

object Concurrent {
  def apply[A](func: (A => Action) => Action) = new Concurrent[A](func)
  def of[A](a: A) = new Concurrent((cont: A => Action) => cont(a))
  
  def stop[A](): Concurrent[A] = Concurrent(_ => Stop())
  def atom[A](ioA: Unit => A): Concurrent[A] = {

    def f(f : A => Action) : Action = Atom(ioA andThen f)

    Concurrent(f)
  }

  def par[A](c1: Concurrent[A], c2: Concurrent[A]): Concurrent[A] = {
    def g(cont: A => Action): Action = {
      Fork(c1.func.apply(cont) , c2.func.apply(cont))
    }

    Concurrent(g)
  }

  private def roundRobin(list: List[Action]): () => Unit = {

    def end(): Unit = {}

    list match {
      case List() => end
      case x :: xs =>
        x match {

          case Atom(cont) =>
            // An Atom monadically executes its argument and puts the resulting
            // process at the back of the process list
            roundRobin(xs ++ List(cont.apply(())))

          case Fork(a1, a2) =>          //        Fork creates two new processes
            roundRobin(xs ++ List(a1,a2))

          case Stop() =>                // and Stop discards its process.
            roundRobin(xs)
        }
    }

   end
  }
}
