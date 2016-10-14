import data.structures.{BTree, BTreeNode}
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, all}
import org.scalacheck.Gen


object BTreeSpecification extends Properties("BTree") {

  val t = 3

  val letters = Gen.listOf(Gen.choose('A', 'z'))

  type Node = BTreeNode[Char, Int]

  def build(items: List[Char]): BTree[Char, Int] =
    items.foldLeft(BTree[Char, Int](3))((t, e) => { t.insert(e, e); t })

  def nodes(bTree: BTree[Char, Int]): List[Node] = {
    def visit(x: Node): List[Node] =
      x::x.children.foldRight(List.empty[Node])(visit(_):::_)

    visit(bTree.root)
  }

  def keys(bTree: BTree[Char, Int]): List[Char] = {
    for {
      n <- nodes(bTree)
      k <- n.keys
    } yield k.key
  }

  property("has a root") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    btree.root != null
  }

  property("has all keys") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val k = keys(btree)

    k.sorted == a.sorted
  }

  property("find") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)

    val test = for {
      k <- a
    } yield btree.find(k).isDefined

    test.forall(v => v)
  }

  property("all keys are in non descending order") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val items = nodes(btree)

    val orderTest = for {
      i <- items
    } yield i.keys.map(k => k.key).sorted == i.keys.map(k => k.key)

    orderTest.forall(v => v)
  }

  property("maximum degree constraint for keys") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val items = nodes(btree)

    val test = for {
      i <- items
    } yield i.keys.length

    test.max <= (2*t - 1)
  }

  property("maximum degree constraint for children") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val items = nodes(btree)

    val test = for {
      i <- items
    } yield i.children.length

    test.max <= 2*t
  }

  property("minimum degree constraint for keys") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val items = nodes(btree)

    val test = for {
      i <- items if i != btree.root
    } yield i.keysCount

    test.isEmpty || test.max >= (t - 1)
  }

  property("minimum degree constraint for children") = forAll(letters) { (a: List[Char]) =>
    val  btree = build(a)
    val items = nodes(btree)

    val test = for {
      i <- items if i != btree.root && !i.leaf
    } yield i.children.length

    ("children max count: " + (if (test.isEmpty) 0 else test.max)) |: all(test.isEmpty || test.max >= t)
  }

  property("node is not present after delete") = forAll(letters) { (a: List[Char]) =>
    val b = a.distinct
    val btree = build(b)

    if(b.nonEmpty) {
      btree.delete(b.head)
      btree.find(b.head).isEmpty
    } else true
  }

  property("after delete all keys are in non descending order") = forAll(letters) { (a: List[Char]) =>
    val b = a.distinct
    val btree = build(b)
    val items = nodes(btree)

    if(b.nonEmpty) {
      btree.delete(b.head)
      val orderTest = for {
        i <- items
      } yield i.keys.map(k => k.key).sorted == i.keys.map(k => k.key)

      orderTest.forall(v => v)
    } else true
  }

  property("after delete maximum degree constraint for keys") = forAll(letters) { (a: List[Char]) =>
    val b = a.distinct
    val btree = build(b)
    val items = nodes(btree)

    if(b.nonEmpty) {
      btree.delete(b.head)
      val test = for {
        i <- items
      } yield i.keys.length

      test.max <= (2*t - 1)
    } else true
  }

  property("after delete minimum degree constraint for keys") = forAll(letters) { (a: List[Char]) =>
    val b = a.distinct
    val btree = build(b)
    val items = nodes(btree)

    if(b.nonEmpty) {
      btree.delete(b.head)
      val test = for {
        i <- items if i != btree.root
      } yield i.keysCount

      test.isEmpty || test.max >= (t - 1)
    } else true
  }

}