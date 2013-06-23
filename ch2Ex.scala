/*
 * | EXERCISE 1 (optional):
 * 
 * Write a function to get the nth Fibonacci number.  The first two
 * Fibonacci numbers are 0 and 1, and the next number is always the sum of
 * the previous two. Your definition should use a local tail-recursive
 * function.
 */

def fib(n: Int): Int = {
	def iter(a: Int, b: Int, c: Int): Int =
		if (c == 0) b
		else iter(a + b, a, c - 1)
	iter(1, 0, n)
}


/*
 * | EXERCISE 2:
 *
 * Implement isSorted, which checks whether an Array[A] is sorted according
 * to a given comparison function.
 */

def isSorted[a](xs: Array[a], gt: (a, a) => Boolean): Boolean = {
	def iter(ys: Array[a], max: a): Boolean =
		if (ys.isEmpty) {
			true
		} else if (gt(ys.head, max)) {
			iter(ys.tail, ys.head)
		} else {
			false
		}
	iter(xs.tail, xs.head)
}


/*
 * | Check whether an int is greater than another int.
 */

def intGt(x: Int, y: Int): Boolean =
	if (x > y) {
		true
	} else {
		false
	}


/*
 * | EXERCISE 3 (hard):
 *
 * Implement partial1 and write down a concrete usage of it. There is only
 * one possible implementation that compiles. We don't have any concrete
 * types here, so we can only stick things together using the local 'rules
 * of the universe' established by the type signature. The style of
 * reasoning required here is very common in functional programming—we are
 * simply manipulating symbols in a very abstract way, similar to how we
 * would reason when solving an algebraic equation.
 */

def partial1[a, b, c](x: a, f: (a, b) => c): b => c =
	y => f(x, y)


/*
 * | Add one to an int.
 */

val addOne: Int => Int = partial1(1, add)


/*
 * | EXERCISE 4 (hard):
 *
 * Let's look at another example, currying, which converts a function of N
 * arguments into a function of one argument that returns another function
 * as its result.11 Here again, there is only one implementation that
 * typechecks.
 */

def curry[a, b, c](f: (a, b) => c): a => (b => c) =
	x => partial1(x, f)


/*
 * | EXERCISE 5 (optional):
 *
 * Implement uncurry, which reverses the transformation of curry. Note that
 * since => associates to the right, A => (B => C) can be written as A => B
 * => C.
 */

def uncurry[a, b, c](f: a => b => c): (a, b) => c =
	(x, y) => f(x)(y)


/*
 * | EXERCISE 6:
 *
 * Implement the higher-order function that composes two functions.
 */

def compose[a, b, c](f: b => c, g: a => b): a => c =
	x => f(g(x))
