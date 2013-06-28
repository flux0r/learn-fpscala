package fpinscala.datastructures

sealed trait Tree[+a]
case class Leaf[a](value: a) extends Tree[a]
case class Branch[a](l: Tree[a], r: Tree[a]) extends Tree[a]

object Tree {

/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 25:
 *
 * Write a function size that counts the number of nodes in a tree.
 */

def size[a](_xs: Tree[a]): Int = _xs match {
	case Leaf(_) => 1
	case Branch(xs, ys) => 1 + size(xs) + size(ys)
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 26:
 *
 * Write a function maximum that returns the maximum element in a Tree[Int].
 */

def maximum(_xs: Tree[Int]): Int = _xs match {
	case Leaf(x) => x
	case Branch(xs, ys) => maximum(xs).max(maximum(ys))
}


}
