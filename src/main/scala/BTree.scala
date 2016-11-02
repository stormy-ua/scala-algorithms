package data.structures

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.compat.Platform._

class BTreeKey[A, B](val key: A, val payload: B) {
  override def toString: String = s"($key, $payload)"
}

class BTreeNode[A, B] {
  var keys = new ArrayBuffer[BTreeKey[A, B]]()
  var children = new ArrayBuffer[BTreeNode[A, B]]()
  var leaf = true

  def keysCount = keys.length
}

class BTree[A, B](t: Int, implicit val cmp: Ordering[A]){
  type Node = BTreeNode[A, B]
  type Key = BTreeKey[A, B]
  type Siblings = (Option[Node], Option[Node])
  type IndicesPair = (Int, Int)

  var root = new Node

  override def toString: String = {
    def nodeToStr(x: Node): String = {
      x.keys.foldRight("")(_.key.toString + "" + _)
    }

    def byDepth(depth: Int, x: Node, p: String): List[(Int, Node, String)] = {
      (depth, x, p) :: x.children.foldRight(List.empty[(Int, Node, String)])((a, b) => byDepth(depth + 1, a, s"${x.keys.head.key}..${x.keys.last.key}"):::b)
    }

    val nodesByDepth = byDepth(0, root, "") sortBy(_._1)
    nodesByDepth.foldRight((-1, "")) ((n, a) => (n._1, (s"|${nodeToStr(n._2)}<-${n._3}| " + (if (n._1 != a._1) EOL else "") + a._2)))._2
  }

  def insert(key: A, payload: B): Unit = {
    val r = root
    if(root.keysCount == (2*t - 1)) {
      val s = new Node
      root = s
      s.leaf = false
      s.children append r

      splitChild(s, 0)
      insertNonFull(s, new Key(key, payload))
    }
    else {
      insertNonFull(r, new Key(key, payload))
    }
  }

  def getKeyAndChildIndex(x: Node, key: A): Option[IndicesPair] =
    x.keysCount match {
      case 0 => None
      case _ => {
        val index = x.keys.lastIndexWhere(v => cmp.compare(v.key, key) < 0) + 1
        Some(math.min(x.keysCount - 1, index), index)
      }
    }

  def getKeyIndex(x: Node, key: A): Option[Int] = getKeyAndChildIndex(x, key) map(p => p._1)

  def getChildIndex(x: Node, key: A) = getKeyAndChildIndex(x, key) map(p => p._2)

  def find(key: A): Option[B] = {
    @tailrec
    def search(x: Node, k: A): Option[B] = {
      getKeyAndChildIndex(x, k) match {
        case Some((i, _)) if cmp.compare(x.keys(i).key, k) == 0 => Some(x.keys(i).payload)
        case _ if x.leaf => None
        case Some((_, i)) => search(x children(i), k)
        case _ => None
      }
    }

    search(root, key)
  }

  def getSiblings(x: Node, key: A): Option[Siblings] =
    for {
      childIndex <- getChildIndex(x, key)
      leftSibling = if (childIndex > 0) Some(x.children(childIndex - 1)) else None
      rightSibling = if (childIndex < (x.children.length - 1)) Some(x.children(childIndex + 1)) else None
    } yield (leftSibling, rightSibling)

  def deleteFromInternalNode(x: Node, keyIndex: Int) = {
    // case 2a
    val key = x.keys(keyIndex).key
    val leftChild = x.children(keyIndex)
    val rightChild = x.children(keyIndex + 1)
    if(leftChild.keysCount >= t){
      val predecessorKey = leftChild.keys.maxBy(_.key)
      x.keys(keyIndex) = predecessorKey
      (leftChild, predecessorKey.key)
    } else if (rightChild.keysCount >= t) {
      // case 2b
      val successorKey = rightChild.keys.minBy(_.key)
      x.keys(keyIndex) = successorKey
      (rightChild, successorKey.key)
    } else {
      // case 2c
      val leftKeys = leftChild.keys
      leftKeys.append(x.keys(keyIndex))
      x.keys.remove(keyIndex)
      x.children.remove(keyIndex + 1)

      leftChild.keys = leftKeys ++ rightChild.keys
      leftChild.children = leftChild.children ++ rightChild.children
      (leftChild, key)
    }
  }

  def addKeyToChild(x: Node, indices: IndicesPair, childNode: Node, siblings: Siblings) = {
    def moveKeyFromRightToLeftSibling(pivotIndex: Int, left: Node, right: Node) = {
      left.keys.append(x.keys(pivotIndex))
      x.keys(pivotIndex) = right.keys.head
      right.keys.remove(0)

      if (right.children.nonEmpty) {
        left.children.append(right.children.head)
        right.children.remove(0)
      }
    }

    def moveKeyFromLeftToRightSibling(pivotIndex: Int, left: Node, right: Node) = {
      right.keys.insert(0, x.keys(pivotIndex))
      x.keys(pivotIndex) = left.keys.last
      left.keys.remove(left.keys.length - 1)

      if(left.children.nonEmpty) {
        childNode.children.insert(0, left.children.last)
        left.children.remove(left.children.length - 1)
      }
    }

    val (keyIndex, childIndex) = indices
    val (leftSibling, rightSibling) = siblings
    (leftSibling, rightSibling) match {
      // case 3a
      case (_, Some(rs)) if rs.keysCount >= t =>
        moveKeyFromRightToLeftSibling(keyIndex, childNode, rs)
      case (Some(ls), _) if ls.keysCount >= t && keyIndex > 0 =>
        moveKeyFromLeftToRightSibling(keyIndex - 1, ls, childNode)
      case (Some(ls), _) if ls.keysCount >= t =>
        moveKeyFromLeftToRightSibling(keyIndex, ls, childNode)
      // case 3b
      case (_, Some(rs)) => {
        childNode.keys.append(x.keys(keyIndex))
        childNode.keys = childNode.keys ++ rs.keys
        childNode.children = childNode.children ++ rs.children
        x.keys.remove(keyIndex)
        x.children.remove(childIndex + 1)
      }
      case (Some(ls), _) => {
        ls.keys.append(x.keys(keyIndex))
        childNode.keys = ls.keys ++ childNode.keys
        childNode.children = ls.children ++ childNode.children
        x.keys.remove(keyIndex)
        x.children.remove(childIndex - 1)
      }
    }
  }

  def delete(key: A): Unit = {
    @tailrec
    def deleteFromNode(x: Node, key: A): Unit = {
      val keyInd = x.keys.indexWhere(_.key == key)
      val keyIndex = if (keyInd > -1) Some(keyInd) else None
      val childIndex = getChildIndex(x, key)

      (keyIndex, childIndex) match {
        case (Some(i), _) if x.leaf => x.keys.remove(i)
        case (Some(i), _) => {
          val (childNode, key) = deleteFromInternalNode(x, i)
          deleteFromNode(childNode, key)
        }
        case (_, Some(ci)) if !x.leaf => {
          val childNode = x.children(ci)

          for {
            indices <- getKeyAndChildIndex(x, key)
            siblings <- getSiblings(x, key)
            if childNode.keysCount == t - 1
          } addKeyToChild(x, indices, childNode, siblings)

          deleteFromNode(childNode, key)
        }
        case _ =>
      }
    }

    if(root.keysCount > 0)
      deleteFromNode(root, key)
  }

  private def splitChild(x: Node, childIndex: Int) = {
    val z = new Node
    val y = x.children(childIndex)
    z.leaf = y.leaf

    z.keys = y.keys drop t

    if (!y.leaf) {
      z.children = y.children drop t
      y.children = y.children take t
    }

    x.children insert(childIndex + 1, z)
    x.keys insert(childIndex, y.keys(t - 1))

    y.keys = y.keys take (t - 1)
  }

  @tailrec
  private def insertNonFull(x: Node, key: Key): Unit = {
    if (x.leaf) {
      val insertAt = x.keys.lastIndexWhere(v => cmp.compare(v.key, key.key) < 0) + 1
      x.keys insert(insertAt, key)
    }
    else {
      var childIndex = x.keys.lastIndexWhere(v => cmp.compare(v.key, key.key) < 0) + 1
      val c = x.children(childIndex)
      if (c.keysCount == (2*t - 1)){
        splitChild(x, childIndex)
        if(cmp.compare(key.key, x.keys(childIndex).key) > 0){
          childIndex += 1
        }
      }

      insertNonFull(x.children(childIndex), key)
    }
  }
}

object BTree {
  def apply[A, B](t: Int)(implicit cmp: Ordering[A]) = new BTree[A, B](t, cmp)
}

object BTreeProgram {
  def main2 (args: Array[String]) {
    val btree = BTree[Char, Int](3)
    //for { l <- 'A' until 'Z' } btree.insert(l, l)

    btree.insert('E', 0)
    btree.insert('A', 0)
    btree.insert('G', 0)
    btree.insert('H', 0)
    btree.insert('I', 0)
    btree.insert('B', 0)
    btree.insert('D', 0)
    btree.insert('F', 0)
    btree.insert('C', 0)
    println(btree)

    btree.delete('E')
    println(btree)
  }
}



