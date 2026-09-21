package intro

import scala.annotation.tailrec

/**
  * This part has some exercises for you to practice with the recursive lists and functions.
  * For the exercises in this part you are _not_ allowed to use library functions,
  * you should implement everything yourself.
  * Use recursion to process lists, iteration is not allowed.
  *
  * This part is worth 16 points.
  */
object Practice {

    /** Q10 (2p)
      * Implement the function that returns the first `n` elements from the list.
      * Note that `n` is an upper bound, the list might not have `n` elements.
      *
      * @param xs list to take items from.
      * @param n amount of items to take.
      * @return the first n items of xs.
      */
    def firstN(xs: List[Int], n: Int): List[Int] = {
        @tailrec
        def helper(rest: List[Int], n: Int, acc: List[Int]): List[Int] = rest match{
            case Nil => acc
            case _ if n <= 0 => acc
            case x :: t => helper(t, n-1, x::acc)
        }
        helper(helper(xs, n, Nil), n, Nil)
    }


    //    def firstN(xs: List[Int], n: Int): List[Int] = xs match{
    //        case Nil => Nil
    //        case _ if n <= 0 => Nil
    //        case x :: t => x::firstN(t, n - 1)
    //    }

    //    recursive option of take(n)

    /** Q11 (4p)
      * Implement the function that returns the maximum value in the list.
      * If the list is empty, return `Int.MinValue`
      *
      * @param xs list to process.
      * @return the maximum value in the list.
      */
    def maxValue(xs: List[Int]): Int = {
        @tailrec
        def helper(rest: List[Int], acc: Int): Int = rest match {
            case Nil => acc
            case x :: t if x > acc => helper(t, x)
            case _ :: t => helper(t, acc)
        }
        helper(xs, Int.MinValue)
    }

    /** Q12 (3p)
     * given two Ints, generate the List[Int] with both numbers inclusive
     *
     * Examples:
     * intList(2,7) // List(2,3,4,5,6,7)
     * intList(3,0) // List()
     */
    def intList(a: Int, b: Int) : List[Int] = {
        @tailrec
        def helper(y: Int, acc: List[Int]): List[Int] = {
            if(y < a) acc
            else if(y == a) a::acc
            else helper(y - 1, y::acc)
        }
        helper(b, Nil)
    }

//    just recursive, not tail recursive

//    def intList(a: Int, b: Int) : List[Int] = {
//        if(a > b)  Nil
//        else if(a == b) a::Nil
//        else a::intList(a+1,b)
//    }

    /**
     * Q13 (7p)
     * This question is a variant on a filter function. Given a List[A] and a
     * function f: A => Boolean, `myFilter` should retain all elements from
     * the list which satisfy 'f' and throw out all other elements. From the
     * resulting list, each element with an odd numbered index should also
     * be thrown out (we start counting the index at 0, of course).
     *
     * Take a look at the examples to see more directly what it needs to do
     * if you find this description vague.
     *
     * You are required to solve this using pattern matching on lists.
     * HINT: define a 'helper' method within myFilter which uses case matching.
     *
     * Examples:
     * 	val nrs = List.range(0,11) // List(0,1,2,3,...,10)
     * 	myFilter(nrs, (i: Int) => i % 2 == 0) // List(0,4,8)
     *
     * so although 2, 6 and 10 satisfy the function, they are thrown out.
     */
    // a helper method which you've written yourself
    def myFilter[A](xs: List[A], f: A => Boolean) : List[A] = {
        def helper(rest: List[A], even: Boolean): List[A] = rest match {
            case Nil => Nil
            case x :: t if f(x) =>
                if (even) x::helper(t, !even)
                else helper(t, !even)
            case _ :: t => helper(t, even)
        }
        helper(xs, true)
    }
}
