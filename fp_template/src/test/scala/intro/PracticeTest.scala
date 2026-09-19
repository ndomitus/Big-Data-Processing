package intro

import Practice._
import org.scalatest.FunSuite

class PracticeTest extends FunSuite {

    test("FirstN") {
        assertResult((1 to 10).toList) {
            firstN((1 to 20).toList, 10)
        }
    }

    test("MaxValue") {
        assertResult(16) {
            maxValue(List(10, 4, 14, -4, 15, 14, 16, 7))
        }
    }

    test("intList") {
        // normal range
        assertResult(List(2, 3, 4, 5, 6, 7))(intList(2, 7))
        // a > b
        assertResult(Nil)(intList(3, 0))
        // a == b
        assertResult(List(5))(intList(5, 5))
        // negative numbers
        assertResult(List(-3, -2, -1))(intList(-3, -1))
        // crossing zero
        assertResult(List(-1, 0, 1))(intList(-1, 1))
        // overflow edge
        assertResult(List(Int.MaxValue))(intList(Int.MaxValue, Int.MaxValue))
        assertResult(List(Int.MaxValue - 1, Int.MaxValue))(intList(Int.MaxValue - 1, Int.MaxValue))
    }

    test("intList large range") {
        assertResult(100000)(intList(1, 100000).length)
    }

    test("myFilter") {
        val isEven = (i: Int) => i % 2 == 0

        // example from the assignment
        assertResult(List(0, 4, 8))(myFilter(List.range(0, 11), isEven))
        // empty list
        assertResult(Nil)(myFilter(Nil, isEven))
        // nothing passes f
        assertResult(Nil)(myFilter(List(1, 3, 5), isEven))
        // everything passes f: only even indexes remain
        assertResult(List(1, 3, 5))(myFilter(List(1, 2, 3, 4, 5), (_: Int) => true))
        // generic type: filtered list is ("a", "c", "e")
        assertResult(List("a", "e"))(myFilter(List("a", "bb", "c", "dd", "e"), (s: String) => s.length == 1))
    }
}
