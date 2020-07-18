package collections

sealed abstract class BinaryTree[+A <% Ordered[A]] {

  def value: A
  def left: BinaryTree[A]
  def right: BinaryTree[A]

  def isEmpty: Boolean
  def size: Int

  def fail(msg: String) = throw new NoSuchElementException(msg)

  override def toString: String =
    if(isEmpty)
      "."
    else
    "{" + left + value + right + "}"

  def add[B >: A <% Ordered[B]](v: B): BinaryTree[B] =
    if(isEmpty)
      BinaryTree.make(v)
    else if(v > value)
      BinaryTree.make(value, left, right.add(v))
    else if(v < value)
      BinaryTree.make(value, left.add(v), right)
    else
      this

  def map[B >: A <% Ordered[B]](f: A => B): BinaryTree[B] =
    if(isEmpty)
      BinaryTree.empty
    else
      BinaryTree.make(f(value), left.map(f), right.map(f))


  def isValid: Boolean = ???

  def isBalanced: Boolean = ???


}

case object Leaf extends BinaryTree[Nothing] {
  override def value = fail("no value in leaf")

  override def left: BinaryTree[Nothing] = fail("no left branch in leaf")

  override def right: BinaryTree[Nothing] = fail("no right branch in leaf")

  override def isEmpty: Boolean = true

  override def size: Int = 0

}

case class TreeBranch[A <% Ordered[A]](value: A, left: BinaryTree[A], right: BinaryTree[A], size: Int) extends BinaryTree[A] {
  override def isEmpty: Boolean = false
}

object BinaryTree{

  def empty[A]: BinaryTree[A] = Leaf

  def apply[A <% Ordered[A]](xs: A*): BinaryTree[A] = {
    var tree: BinaryTree[A] = empty
    for(x <- xs)
      tree = tree.add(x)
    tree
  }

  def make[A <% Ordered[A]](x: A, l: BinaryTree[A] = Leaf, r: BinaryTree[A] = Leaf) =
    TreeBranch(x, l, r, l.size + r.size + 1)

}