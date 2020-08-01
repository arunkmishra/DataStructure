package collections

abstract class List[+A] {

  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def fail(msg: String) = throw new RuntimeException(msg)

  def append[B >: A](x: B): List[B] =
    if (isEmpty)
      List.create(x, Nil)
    else
      List.create(head, tail.append(x))

  def foreach(f: A => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  override def toString: String =
    if(isEmpty)
      "øEND"
    else
      s"$head -> ${tail.toString}"

  def prepend[B >:A](x: B): List[B] =
    List.create(x, this)
  
  def size: Int = {
    def s(c: Int, ls: List[A]): Int =
      if(ls.isEmpty)
        c
      else
        s(c+1, ls.tail)
    s(0, this)
  }

  def apply(x: Int): A =
    if(isEmpty) fail("Empty List")
    else if(x < 0) fail("Index out of bound")
    else if(x == 0) head
    else
      tail(x - 1)

  def <->[B >:A](otherList: List[B]): List[B] =
    if(isEmpty)
      otherList
    else
      tail.<->(otherList).prepend(head)

  def remove[B >: A](x: B): List[B] =
    if(isEmpty)
      fail(s"can't find $x in list")
    else if(head != x)
      List.create(head, tail.remove(x))
    else
      tail

  def contains[B >: A](x: B): Boolean =
    if(isEmpty)
      false
    else if(head == x)
      true
    else
      tail.contains(x)

  def suffixes: List[List[A]] =
    if (isEmpty) List.create(List.empty)
    else tail.suffixes.prepend(this)

  def fold[B >: A](zero: B)(op: (B, A) => B): B = {
    def loop(res: B, ls: List[A]): B =
      ls match {
        case ->(h, t) => loop(op(res, h), t)
        case Nil => res
      }
    loop(zero, this)
  }

  def map[B >: A](f: A => B): List[B] =
    if(isEmpty)
      List.empty
    else
      ->(f(head), tail.map(f))

  def sum[B >: A](implicit num: Numeric[B]): B =
    fold(num.zero)(num.plus)

  def product[B >: A](implicit num: Numeric[B]): B =
    fold(num.one)(num.times)

  def min[B >: A](implicit ordering: Ordering[B]): B =
    if(isEmpty)
      fail("empty list")
    else if (tail.isEmpty)
      head
    else
      ordering.min(head, tail.min(ordering))

}

case object Nil extends List[Nothing] {

  override def head: Nothing = fail("No head for empty List")
  override def tail: List[Nothing] = fail("No tail for empty list")
  override def isEmpty: Boolean = true
}

case class ->[A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
}

object List {

  def empty[A]: List[A] = Nil

  def create[A](firstElement: A, restElements: List[A] = Nil): List[A] =
    ->(firstElement, restElements)

  def apply[A](elements: A*): List[A] = {
    var ls: List[A] = List.empty
    for (l <- elements)
      ls = ls.append(l)
    ls
  }
}

