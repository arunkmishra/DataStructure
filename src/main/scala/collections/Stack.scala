package collections

class Stack[+T](self: List[T]) {

  def top: T = self.head

  def isEmpty: Boolean = self.isEmpty

  def rest: Stack[T] = new Stack[T](self.tail)

  def push[B >: T ](x: B): Stack[B] = new Stack[B](self.prepend(x))

  def pop: Stack[T] = new Stack[T](self.tail)

  def size: Int = self.size

  def printStack = println(self.toString)

}

object Stack {

  def empty[A]: Stack[A] = new Stack[A](Nil)

  def apply[A](xs: A*): Stack[A] =
    xs.foldLeft(Stack.empty[A])((r,a) => r.push(a))

}