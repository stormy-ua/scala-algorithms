package algorithms.trees

import scala.annotation.tailrec

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case object End extends Tree[Nothing]

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

trait Foldable[F[_]] {
  def foldLeft[A, B](a: F[A], z: B)(f: (A, B) => B): B
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Shows {
  trait Show[A] {
    def shows(a: A): String
  }

  class TreeShow[A] extends Show[Tree[A]] {
    def shows(a: Tree[A]): String = {
      a match {
        case End => "."
        case Node(v, left, right) => s"T($v ${shows(left)} ${shows(right)})"
      }
    }
  }

  implicit val intTreeShow = new TreeShow[Int]
  implicit val stringTreeShow = new TreeShow[String]

  implicit val listShow = new Show[List[Int]] {
    override def shows(a: List[Int]): String = a.toString
  }

  implicit def show[A: Show](a: A): String = implicitly[Show[A]].shows(a)

  implicit class ShowOps[A](val a: A) extends AnyVal {
    def show(implicit s: Show[A]): String = s.shows(a)
    def println(implicit s: Show[A]) = scala.Predef.println(show(s))
  }
}


object TreesProgram extends App {
  import Shows._

  val foldableTree = new Foldable[Tree] {
    def foldLeft[A, B](a: Tree[A], z: B)(f: (A, B) => B): B =
      a match {
        case End => z
        case Node(v, left, right) => {
          val zl = foldLeft(left, z)(f)
          val zin = f(v, zl)
          foldLeft(right, zin)(f)
        }
      }
  }

  val mapableTree = new Functor[Tree] {
    override def map[A, B](a: Tree[A])(f: (A) => B): Tree[B] =
      a match {
        case Node(v, left, right) => Node(f(v), map(left)(f), map(right)(f))
        case End => End
      }
  }

  val binarySearchTree: Tree[Int] = Node(6, Node(5, Node(2), Node(5)), Node(7, End, Node(8)))
  binarySearchTree.println

  // fold
  foldableTree.foldLeft(binarySearchTree, List.empty[Int])(_ :: _).println

  // map over the tree using functor
  mapableTree.map(binarySearchTree)(_ + 10).println
}
