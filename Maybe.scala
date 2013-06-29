package fpinscala.error
/*--------------------------------------------------------------------------*/



/*----------------------------------------------------------------------------
 * | Represent computations that may not always have a value.
 */

sealed trait Maybe[+a] {


/*----------------------------------------------------------------------------
 * | Chapter 4 EXERCISE 1:
 *
 * We'll explore when you'd use each of these next. But first, as an exercise,
 * implement all of the above functions on Option. As you implement each
 * function, try to think about what it means and in what situations you'd use
 * it.
 */ 

def map[b](f: a => b): Maybe[b] = this match {
	case Nothing => Nothing
	case Just(x) => Just(f(x))
}


def bind[b](f: a => Maybe[b]): Maybe[b] = this match {
	case Nothing => Nothing
	case Just(x) => f(x)
}


def extract[b >: a](default: => b): b = this match {
	case Nothing => default
	case Just(x) => x
}


def orElse[b >: a](y: => Maybe[b]): Maybe[b] =
	this.map(Just(_)).extract(y)


def filter(p: a => Boolean): Maybe[a] = this match {
	case Just(x) =>
		if (p(x)) {
			this
		} else {
			Nothing
		}
	case _	=> Nothing
}


}

case class Just[+a](just: a) extends Maybe[a]
case object Nothing extends Maybe[Nothing]
/*--------------------------------------------------------------------------*/


object Maybe {
/*----------------------------------------------------------------------------
 * | Chapter 4 EXERCISE 2:
 *
 * Implement the variance function (if the mean is m, variance is the mean of
 * math.pow(x - m, 2), see definition) in terms of mean and flatMap.
 */

def mean(xs: Seq[Double]): Maybe[Double] =
	if (xs.isEmpty) {
		Nothing
	} else {
		Just(xs.sum/xs.length)
	}

def variance(xs: Seq[Double]): Maybe[Double] =
	mean(xs).bind(y => mean(xs.map(x => math.pow(x - y, 2))))


}
