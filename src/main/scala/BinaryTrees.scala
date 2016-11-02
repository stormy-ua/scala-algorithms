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

val binarySearchTree = Node(6, Node(5, Node(2), Node(5)), Node(7, End, Node(8)))

def inOrderFold[T, V](t: Tree[T], a: V) (f: (V, T) => V): V = {
	t match {
		case End => a
		case Node(v, left, right) => {
			val al = inOrderFold(left, a)(f)
			val ain = f(al, v)
			inOrderFold(right, ain)(f)			
		}
	}	
}