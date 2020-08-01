import tree.BinaryTree
import collections.List

object CollectionRunner {

    def main(args: Array[String]): Unit = {
      println("--------- Binary Tree ------------")
      binaryTreeRunner()
      println("----------------------------------")
      println("----------- LIST ------------")
      listRunner()
      println("----------------------------------")

    }

  def binaryTreeRunner(): Unit = {
    val intTree: BinaryTree[Int] = BinaryTree(6, 10, 4, 5)
    val charTree: BinaryTree[Char] = BinaryTree('a', 'b', 'b')

    println("toString :" + intTree)

    println("map: " + intTree.map(i => i * 2))
    println("map: " + charTree.map(i => i.toUpper))
  }

  def listRunner(): Unit = {
    //This list is immutable
    val ls = List(1,2,3,4)

    println(ls) //1 -> 2 -> 3 -> 4 -> øEND
    println(ls.append(5))//1 -> 2 -> 3 -> 4 -> 5 -> øEND
    println(ls.prepend(0))//0 -> 1 -> 2 -> 3 -> 4 -> øEND
    println(ls.size)
    println(ls(2))

    val xs = List(5,6,7,8)
    println(ls <-> xs)

    //implicit val ordering = Ordering[Int]

    println(ls.remove(1))
    println(ls.contains(3))
    println(ls.suffixes)
    println(ls.fold(1)(_ * _))
    println(ls.map(i => i * 2))
    println(ls.sum)
    println(ls.product)
    println(ls.min)
  }

}
