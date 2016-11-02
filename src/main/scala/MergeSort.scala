package algorithms.sorting

import scala.annotation.tailrec

def mergeSort(a: List[Int]): List[Int] = {
	def merge(x: List[Int], y: List[Int]): List[Int] = {
		@tailrec
		def go(x: List[Int], y: List[Int], merged: List[Int]): List[Int] =
			(x, y) match {
				case (hx::tx, hy::ty) if hx > hy => go(x, ty, hy::merged)
				case (hx::tx, hy::ty) if hx < hy => go(tx, y, hx::merged)
				case (Nil, hy::ty) => go(x, ty, hy::merged)
				case (hx::tx, Nil) => go(tx, y, hx::merged)
				case _ => merged reverse
			}

		go(x, y, List.empty)
	}

	val size = a.size
	if(size > 1) {
		val (left, right) = a.splitAt(size/2)
		merge(mergeSort(left), mergeSort(right))
	}
	else a
}