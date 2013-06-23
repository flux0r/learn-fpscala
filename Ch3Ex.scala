import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
import fpinscala.datastructures.List._

/*
 * EXERCISE 1:
 *
 * What will the result of the following match expression be?
 */

val x = List(1,2,3,4,5) match {
	case Cons(x, Cons(2, Cons(4, _))) => x
	case Nil => 42
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
 * | EXERCISE 2: See List.scala.
 */


/*
 * | EXERCISE 3: See List.scala.
 */


/*
 * | EXERCISE 4: See List.scala.
 */


/*
 * | EXERCISE 5: See List.scala.
 */


/*
 * | EXERCISE 6: See List.scala.
 */


/*
 * | EXERCISE 7:
 *
 * Can product implemented using foldRight immediately halt the recursion
 * and return 0.0 if it encounters a 0.0? Why or why not? Consider how any
 * short-circuiting might work if you call foldRight with a large list. This
 * is a deeper question that we'll return to a few chapters from now.
 */

/* 
 * foldRight can't stop prematurely because Scala is normal-order by default
 * and the function will not be called until the fold traverses the entire
 * data structure.
 */


/*
 * | EXERCISE 8:
 *
 * See what happens when you pass Nil and Cons themselves to foldRight, like
 * this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think
 * this says about the relationship between foldRight and the data
 * constructors of List?
 */

/*
 * Running this gives me the same list back. foldRight is essentially
 * replacing Nils with the starting element and Conses(?) with the function
 * operating on the data structure given to to it.
 */


/*
 * | EXERCISE 9: See List.scala.
 */


/*
 * | EXERCISE 10: See List.scala.
 */
