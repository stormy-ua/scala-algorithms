//package algorithms.trees

import scala.annotation.tailrec

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

trait Foldable[F[_]] {
	def foldLeft[A, B](a: F[A], z: B)(f: (A, B) => B): B
}

var foldableTree = new Foldable[Tree] {
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

val binarySearchTree = Node(6, Node(5, Node(2), Node(5)), Node(7, End, Node(8)))
foldableTree.foldLeft(binarySearchTree, List.empty[Int])(_::_)
