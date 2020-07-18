import collections.BinaryTree

object BinaryTreeRunner {

    def main(args: Array[String]): Unit = {
      val intTree: BinaryTree[Int] = BinaryTree(6, 10, 4, 5)
      val charTree: BinaryTree[Char] = BinaryTree('a', 'b', 'b')

      println("toString :" + intTree)

      println("map: " + intTree.map(i => i * 2))
      println("map: " + charTree.map(i => i.toUpper))
    }

}
