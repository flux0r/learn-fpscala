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


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 27:
 *
 * Write a function depth that returns the maximum path length from the root
 * of a tree to any leaf.
 */

def depth[a](_xs: Tree[a]): Int = _xs match {
	case Leaf(_) => 0
	case Branch(xs, ys) => 1 + depth(xs).max(depth(ys))
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 28:
 *
 * Write a function map, analogous to the method of the same name on List,
 * that modifies each element in a tree with a given function.
 */

def map[a, b](_xs: Tree[a])(f: a => b): Tree[b] = _xs match {
	case Leaf(x) => Leaf(f(x))
	case Branch(xs, ys) => Branch(map(xs)(f), map(ys)(f))
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 29:
 *
 * Generalize size, maximum, depth, and map, writing a new function fold that
 * abstracts over their similarities. Reimplement them in terms of this more
 * general function. Can you draw an analogy between this fold function and
 * the left and right folds for List?
 */

def fold[a, b](_xs: Tree[a])(f: a => b)(g: (b, b) => b): b = _xs match {
	case Leaf(x) => f(x)
	case Branch(xs, ys) => g(fold(xs)(f)(g), fold(ys)(f)(g))
}
	
def sizeByFold[a](_xs: Tree[a]): Int =
	fold(_xs)((_) => 1)((x, y) => 1 + x + y)
	
def maximumByFold(_xs: Tree[Int]): Int =
	fold(_xs)((x) => x)((x, y) => x.max(y))

def depthByFold[a](_xs: Tree[a]): Int =
	fold(_xs)((_) => 0)((x, y) => 1 + x.max(y))

def mapByFold[a, b](_xs: Tree[a])(f: a => b): Tree[b] =
	fold(_xs)((x) => Leaf(f(x)): Tree[b])((x, y) => Branch(x, y))


/*----------------------------------------------------------------------------
 * | Example trees.
 */

val example = Branch(Branch(Leaf(4), Branch(Leaf(8), Leaf(10))),
	Branch(Branch(Leaf(16), Leaf(21)), Leaf(31)))

}
