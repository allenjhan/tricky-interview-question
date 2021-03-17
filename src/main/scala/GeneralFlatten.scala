

object GeneralFlatten extends App{

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List{
    def flatten(l: List[Any]): List[Any] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) => gunion(head, flatten(tail))
      }
    }

    def foldRight[T, U](l: List[T], acc: U)(fun: (T, U) => U): U = {
      l match {
        case Nil => acc
        case Cons(x, xs) => fun(x, foldRight(xs, acc)(fun))
      }
    }

    def union[T](l1: List[T], l2: List[T]): List[T] = {
      foldRight(l1, l2){ (e, acc) =>
        Cons(e, acc)
      }
    }

    def gunion(l1: Any, l2: List[Any]): List[Any] = {
      l1 match {
        case Nil => l2
        case e1 @ Cons(x, xs) => union(e1, l2)
        case x => Cons(x, l2)
      }
    }

  }

  import List.flatten

  val myList = Cons(Cons(1, Cons(2, Nil)), Cons(3, Cons(Cons(4, Cons(5, Nil)), Cons(6, Cons(Cons(7,Nil), Nil)))))

  println(flatten(myList))

}