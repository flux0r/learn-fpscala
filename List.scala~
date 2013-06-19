package fpinscala.datastructures

sealed trait List[a]
case class Nil[a]() extends List[a]
case class Cons[a](head: a, tail: List[a]) extends List[a]

object List {
	def tail[a](xs: List[a]): List[a] = xs match {
		case Nil() => Nil()
		case Cons(x_, xs_) => xs_
	}

	def drop[a](xs: List[a], n: Int): List[a] = {
		if (n <= 0) {
			xs
		} else {
			drop(tail(xs), n - 1)
		}
	}

	def sum(xs: List[Int]): Int = xs match {
		case Nil() => 0
		case Cons(x, xs_) => x + sum(xs_)
	}

	def product(xs: List[Double]): Double = xs match {
		case Nil() => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs_) => x*product(xs_)
	}

	def apply[a](xs: a*): List[a] = {
		if (xs.isEmpty) {
			Nil()
		} else {
			Cons(xs.head, apply(xs.tail: _*))
		}
	}

	val example = Cons(1, Cons(2, Cons(3, Nil())))
	val example2 = List(1, 2, 3)
	val total = sum(example)

}
