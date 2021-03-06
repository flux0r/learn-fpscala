package fpinscala.datastructures

sealed trait List[+a]
case object Nil extends List[Nothing]
case class Cons[+a](head: a, tail: List[a]) extends List[a]


object List {

/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 2:
 *
 * Implement the function tail for "removing" the first element of a List.
 * Notice the function takes constant time. What are different choices you
 * could make in your implementation if the List is Nil?  We will return to
 * this question in the next chapter.
 */

def tail[a](xs: List[a]): List[a] = xs match {
	case Nil => Nil
	case Cons(x_, xs_) => xs_
}

/*
 * I could have thrown an error instead of giving back the Nil List.
 */


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 3:
 *
 * Generalize tail to the function drop, which removes the first n elements
 * from a List.
 */

def drop[a](xs: List[a], n: Int): List[a] = {
	if (n <= 0) {
		xs
	} else {
		drop(tail(xs), n - 1)
	}
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 4:
 *
 * Implement dropWhile, which removes elements from the List prefix as long
 * as they match a predicate. Again, notice these functions take time
 * proportional only to the number of elements being dropped—we do not need
 * to make a copy of the entire List.
 */

def dropWhile[a](_xs: List[a])(p: a => Boolean): List[a] =
	_xs match {
		case Nil => Nil
		case Cons(x, xs) => if (p(x)) {
			dropWhile(xs)(p)
		} else {
			_xs
		}
	}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 5:
 *
 * Using the same idea, implement the function setHead for replacing the
 * first element of a List with a different value.
 */

def setHead[a](_xs: List[a], _x: a): List[a] = _xs match {
	case Nil => Cons(_x, _xs)
	case Cons(x, xs) => Cons(_x, xs)
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 6:
 *
 * Not everything works out so nicely. Implement a function, init, which
 * returns a List consisting of all but the last element of a List. So,
 * given List(1,2,3,4), init will return List(1,2,3). Why can't this
 * function be implemented in constant time like tail?
 */

def init[a](_xs: List[a]): List[a] = _xs match {
	case Nil => Nil
	case Cons(_, Nil) => Nil
	case Cons(x, xs) => Cons(x, init(xs))
}

/*
 * init can't be implemented in constant time because the constructors for
 * List only let me break a List into the head and tail. This means I have
 * to traverse the entire structure, copying all along, to get to the end.
 */


/*--------------------------------------------------------------------------*/

def sum(xs: List[Int]): Int = xs match {
	case Nil => 0
	case Cons(x, xs_) => x + sum(xs_)
}


def product(xs: List[Double]): Double = xs match {
	case Nil => 1.0
	case Cons(0.0, _) => 0.0
	case Cons(x, xs_) => x*product(xs_)
}


def apply[a](xs: a*): List[a] = {
	if (xs.isEmpty) {
		Nil
	} else {
		Cons(xs.head, apply(xs.tail: _*))
	}
}


def foldRight[a, b](_xs: List[a], z: b)(f: (a, b) => b): b =
	_xs match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 9:
 *
 * Compute the length of a list using foldRight.
 */

def length[a](xs: List[a]): Int =
        foldRight(xs, 0)((_, c) => c + 1)


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 10:
 *
 * foldRight is not tail-recursive and will StackOverflow for large lists.
 * Convince yourself that this is the case, then write another general
 * list-recursion function, foldLeft that is tail-recursive, using the
 * techniques we discussed in the previous chapter.
 */

/*
 * foldRight isn't tail-recursive because there is still evaluation of the
 * function left after the recursive calls return. Eventually, I'll run out
 * of stack space for the unevaluated functions.
 */

def foldLeft[a, b](_xs: List[a], z: b)(f: (b, a) => b): b =
	_xs match {
		case Nil =>  z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 11:
 *
 * Write sum, product, and a function to compute the length of a list using
 * foldLeft.
 */

def leftSum(_xs: List[Int]): Int =
	foldLeft(_xs, 0)((x, y) => x + y)

def leftProduct(_xs: List[Double]): Double =
	foldLeft(_xs, 1.0)((x, y) => x*y)

def leftLength[a](_xs: List[a]): Int =
	foldLeft(_xs, 0)((c, _) => c + 1)


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 12:
 *
 * Write a function that returns the reverse of a list (so given List(1, 2,
 * 3), it returns List(3, 2, 1)). See if you can write using a fold.
 */

def reverse[a](_xs: List[a]): List[a] =
	foldLeft(_xs, Nil: List[a])((xs, x) => Cons(x, xs))


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 13 (hard):
 *
 * Can you write foldLeft in terms of foldRight? How about the other way
 * around?
 */

def foldLeftByFoldRight[a, b](_xs: List[a], z: b)(f: (b, a) => b): b = {
	def id(x: b): b = x
	def combiner(x: a, g: b => b) = (y: b) => g(f(y, x))
	foldRight(_xs, id(_))(combiner)(z)
}

def foldRightByFoldLeft[a, b](_xs: List[a], z: b)(f: (a, b) => b): b = {
	def flip[a, b, c](g: (a, b) => c): (b, a) => c =
		(x, y) => g(y, x)
	foldLeft(reverse(_xs), z)(flip(f))
}
	
/*
 * | Showing equivalence of foldLeft and foldLeftByFoldRight.
 *
 * foldLeft(Cons(1, Cons(2, Nil)), 0)(_+_)
 * foldLeft(Cons(2, Nil), (0 + 1))(_+_)
 * foldLeft(Nil, ((0 + 1) + 2))(_+_)
 * ((0 + 1) + 2)
 * 
 * foldLeftByFoldRight(Cons(1, Cons(2, Nil)), 0)(_+_)
 * foldRight(Cons(1, Cons(2, Nil)), id)(combiner)(0)
 * combiner(1, foldRight(Cons(2, Nil), id)(combiner))(0)
 * combiner(1, combiner(2, foldRight(Nil, id)(combiner)))(0)
 * combiner(1, combiner(2, id))(0)
 * ((y1: b) => combiner(2, id)(y1 + 1))(0)
 * ((y1: b) => ((y0: b) => id(y0 + 2))(y1 + 1))(0)
 * ((y0:b) => id(y0 + 2))(0 + 1)
 * id((0 + 1) + 2)
 * ((0 + 1) + 2)
 *
 */

/* 
 * | Showing equivalence of foldRight and foldRightByFoldLeft.
 *
 * foldRight(Cons(1, Cons(2, Nil)), 0)(_+_)
 * (1 + foldRight(Cons(2, Nil), 0)(_+_))
 * (1 + (2 + foldRight(Nil, 0)(_+_)))
 * (1 + (2 + 0))
 * 
 * 
 * foldRightByFoldLeft(Cons(1, Cons(2, Nil)), 0)(_+_)
 * foldLeft(reverse(Cons(1, Cons(2, Nil))), 0)(flip(f))
 * foldLeft(foldLeft(Cons(1, Cons(2, Nil)), Nil)((xs, x) => Cons(x, xs)),
 *	0)(flip(f))
 * foldLeft(foldLeft(Cons(2, Nil), Cons(1, Nil))((xs, x) => Cons(x, xs)),
 * 	0)(flip(f))
 * foldLeft(foldLeft(Nil, Cons(2, Cons(1, Nil)))((xs, x) => Cons(x, xs)),
 * 	0)(flip(f))
 * foldLeft(Cons(2, Cons(1, Nil)), 0)(flip(f))
 * foldLeft(Cons(2, Cons(1, Nil)), 0)((x, y) => (y + x))
 * foldLeft(Cons(1, Nil), (2 + 0))((x, y) => (y + x))
 * foldLeft(Nil, (1 + (2 + 0)))((x, y) => (y + x))
 * (1 + (2 + 0))
 * 
 */


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 14:
 *
 * Implement append in terms of either foldLeft or foldRight.
 */

def append[a](_xs: List[a], _ys: List[a]): List[a] =
	foldRight(_xs, _ys)(Cons(_, _))

/*
 * | Tracing append.
 *
 * append(Cons(1, Cons(2, Nil)), Cons(6, Nil))
 * foldRight(Cons(1, Cons(2, Nil)), Cons(6, Nil))(Cons(_, _))
 * Cons(1, foldRight(Cons(2, Nil), Cons(6, Nil)))(Cons(_, _))
 * Cons(1, Cons(2, foldRight(Nil, Cons(6, Nil))))(Cons(_, _))
 * Cons(1, Cons(2, Cons(6, Nil)))
 */


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 15 (hard):
 *
 * Write a function that concatenates a list of lists into a single list. Its
 * runtime should be linear in the total length of all lists. Try to use
 * functions we have already defined.
 */

def join[a](_xs: List[List[a]]): List[a] =
	foldLeft(_xs, Nil: List[a])(append)

/*
 * | Tracing join.
 *
 * join(Cons(Cons(2, Cons(3, Nil)), Cons(Cons(5, Nil), Nil)))
 * foldLeft(Cons(Cons(2, Cons(3, Nil)), Cons(Cons(5, Nil), Nil)), Nil)(append)
 * foldLeft(Cons(Cons(5, Nil), Nil),
 *	append(Nil, Cons(2, Cons(3, Nil))))(append)
 * foldLeft(Nil, append(append(Nil, Cons(2, Cons(3, Nil))),
 *	Cons(5, Nil)))(append)
 * append(append(Nil, Cons(2, Cons(3, Nil))), Cons(5, Nil))
 * append(foldRight(Nil, Cons(2, Cons(3, Nil)))(Cons(_, _)), Cons(5, Nil))
 * append(Cons(2, Cons(3, Nil)), Cons(5, Nil))
 * foldRight(Cons(2, Cons(3, Nil)), Cons(5, Nil))(Cons(_, _))
 * Cons(2, foldRight(Cons(3, Nil), Cons(5, Nil))(Cons(_, _)))
 * Cons(2, Cons(3, foldRight(Nil, Cons(5, Nil)))(Cons(_, _)))
 * Cons(2, Cons(3, Cons(5, Nil)))
 */


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 16:
 *
 * Write a function that transforms a list of integers by adding 1 to each
 * element. (Reminder: this should be a pure function that returns a new
 * List!)
 */

def mapAddOne(_xs: List[Int]): List[Int] = {
	def addOne(x: Int): Int = x + 1
	foldRight(_xs, Nil: List[Int])((x, y) => Cons(addOne(x), y))
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 17:
 *
 * Write a function that turns each value in a List[Double] into a String.
 */

def mapDoubleToString(_xs: List[Double]): List[String] =
	foldRight(_xs, Nil: List[String])((x, y) => Cons(x.toString, y))


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 18:
 *
 * Write a function map, that generalizes modifying each element in a list
 * while maintaining the structure of the list. Here is its signature:
 */

def map[a, b](_xs: List[a])(f: a => b): List[b] =
	foldRight(_xs, Nil: List[b])((x, y) => Cons(f(x), y))


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 19:
 *
 * Write a function filter that removes elements from a list unless they
 * satisfy a given predicate. Use it to remove all odd numbers from a
 * List[Int].
 */

def filter[a](_xs: List[a])(p: a => Boolean): List[a] = {
	def f(x: a, xs: List[a]): List[a] = {
		if (p(x)) {
			Cons(x, xs)
		} else {
			xs
		}
	}
	
	foldRight(_xs, Nil: List[a])(f)
}

val filterExample = filter(List(1, 2, 3, 4, 5, 6))((x) => x%2 == 0)

/* 
 * scala> List.filterExample
 * res0: fpinscala.datastructures.List[Int] = Cons(2,Cons(4,Cons(6,Nil)))
 */


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 20:
 *
 * Write a function flatMap, that works like map except that the function
 * given will return a list instead of a single result, and that list should
 * be inserted into the final resulting list. Here is its signature:
 */

def flatMap[a, b](_xs: List[a])(f: a => List[b]): List[b] =
	join(map(_xs)(f))


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 21:
 *
 * Can you use flatMap to implement filter?
 */

def filterByFlatMap[a](_xs: List[a])(p: a => Boolean): List[a] = {
	def f(x: a): List[a] = {
		if (p(x)) {
			Cons(x, Nil)
		} else {
			Nil
		}
	}
	flatMap(_xs)(f)
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 22:
 *
 * Write a function that accepts two lists and constructs a new list by adding
 * corresponding elements. For example, List(1, 2, 3) and List (4, 5, 6)
 * becomes List(5, 7, 9).
 */

def addPairs(_xs: List[Int], _ys: List[Int]): List[Int] = _xs match {
	case Nil => Nil
	case Cons(x, xs) => _ys match {
		case Nil => Nil
		case Cons(y, ys) => Cons(x + y, addPairs(xs, ys))
	}
}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 23:
 *
 * Generalize the function you just wrote so that it's not specific to
 * integers or addition.
 */

def zip[a, b, c] (_xs: List[a], _ys: List[b]) (f: (a, b) => c): List[c] =
	_xs match {
		case Nil => Nil
		case Cons(x, xs) => _ys match {
			case Nil => Nil
			case Cons(y, ys) => Cons(f(x, y), zip(xs, ys)(f))
		}
	}


/*----------------------------------------------------------------------------
 * | Chapter 3 EXERCISE 24 (hard):
 *
 * As an example, implement hasSubsequence for checking whether a List
 * contains another List as a subsequence. For instance, List(1,2,3,4) would
 * have List(1,2), List(2,3), and List(4) as subsequences, among others. You
 * may have some difficulty finding a concise purely functional implementation
 * that is also efficient. That's okay. Implement the function however comes
 * most naturally. We will return to this implementation in a couple of
 * chapters and hopefully improve on it. Note: any two values, x, and y, can
 * be compared for equality in Scala using the expression x == y.
 */

def hasSubsequence[a](_xs: List[a], _ys: List[a]): Boolean = {

	def prefix(_xs: List[a], _ys: List[a]): Boolean = _xs match {
		case Nil => false
		case Cons(x, xs) => _ys match {
			case Nil => true
			case Cons(y, ys) => if (x == y) {
				prefix(xs, ys)
			} else {
				false
			}
		}
	}
	
	def iter(_xs: List[a]): Boolean = _xs match {
		case Nil => false
		case Cons(x, xs) => if (prefix(_xs, _ys)) {
			true
		} else {
			iter(xs)
		}
	}
	
	iter(_xs)
}


/*--------------------------------------------------------------------------*/

val example = Cons(1, Cons(2, Cons(3, Nil)))
val example2 = List(1, 2, 3)
val total = sum(example)

}
