package u03.extensionmethods

import scala.annotation.tailrec

object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _ => 0

      @tailrec
      def foldLeft(default: Int)(f: (Int, Int) => Int): Int = l match
        case Cons(h, t) => t.foldLeft(f(default, h))(f)
        case Nil() => default


    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil() => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t) => t.filter(pred)
        case Nil() => Nil()
      
      
    def of[A](n: Int, a: A): Sequence[A] =
      if n == 0 then Nil[A]() else Cons(a, of(n - 1, a))
  end Sequence
end Sequences

@main def trySequences() =
  import Sequences.*
  import Sequence.*

  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(lst.foldLeft(0)(_ - _)) //-16
