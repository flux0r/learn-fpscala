import fpinscala.datastructures._
import fpinscala.datastructures.List._

/*
 * EXERCISE 1:
 *
 * What will the result of the following match expression be?
 */

val x = List(1,2,3,4,5) match {
	case Cons(x, Cons(2, Cons(4, _))) => x
	case Nil() => 42
	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
	case Cons(h, t) => h + sum(t)
	case _ => 101
}


/*
 * The value will be 3. The first case matches Cons(x, Cons(2, but doesn't
 * match Cons(4. The second case is the wrong constructor. The third case
 * matches completely and assigns 1 to x and 2 to y; their sum is 3.
 */

/*
 * EXERCISE 2: See List.scala.
 */
