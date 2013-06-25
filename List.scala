package fpinscala.datastructures

sealed trait List[+a]
case object Nil extends List[Nothing]
case class Cons[+a](head: a, tail: List[a]) extends List[a]


object List {
/* 
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


/*
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


/*
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


/*
 * | Chapter 3 EXERCISE 5:
 *
 * Using the same idea, implement the function setHead for replacing the
 * first element of a List with a different value.
 */

def setHead[a](_xs: List[a], _x: a): List[a] = _xs match {
	case Nil => Cons(_x, _xs)
	case Cons(x, xs) => Cons(_x, xs)
}


/*
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


/*
 * | EXERCISE 9:
 *
 * Compute the length of a list using foldRight.
 */

def length[a](xs: List[a]): Int =
        foldRight(xs, 0)((_, c) => c + 1)


/*
 * | EXERCISE 10:
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


/*
 * | EXERCISE 11: Write sum, product, and a function to compute the length
 * of a list using foldLeft.
 */

def leftSum(_xs: List[Int]): Int =
	foldLeft(_xs, 0)((x, y) => x + y)

def leftProduct(_xs: List[Double]): Double =
	foldLeft(_xs, 1.0)((x, y) => x*y)

def leftLength[a](_xs: List[a]): Int =
	foldLeft(_xs, 0)((c, _) => c + 1)


/*
 * | EXERCISE 12: Write a function that returns the reverse of a list (so
 * given List(1, 2, 3), it returns List(3, 2, 1)). See if you can write using
 * a fold.
 */

def reverse[a](_xs: List[a]): List[a] =
	foldLeft(_xs, Nil: List[a])((xs, x) => Cons(x, xs))


/*
 * | EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight? How
 * about the other way around?
 */

def foldLeftByFoldRight[a, b](_xs: List[a], z: b)(f: (b, a) => b): b =
	foldRight(_xs, (x: b) => x)((y, g) => x => g(f(x, y)))(z)


val example = Cons(1, Cons(2, Cons(3, Nil)))
val example2 = List(1, 2, 3)
val total = sum(example)

}
