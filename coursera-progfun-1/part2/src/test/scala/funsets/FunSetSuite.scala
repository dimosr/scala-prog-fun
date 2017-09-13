package funsets

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite with BeforeAndAfter {
  import FunSets._

  var s1: Set = _
  var s2: Set = _
  var s3: Set = _
  var s12: Set = _
  var s23: Set = _
  var s123: Set = _

  before {
    s1 = singletonSet(1)
    s2 = singletonSet(2)
    s3 = singletonSet(3)
    s123 = union(union(s1, s2), s3)
    s12 = union(s1, s2)
    s23 = union(s2, s3)
  }

  test("singletonSet(1) contains 1") {
    assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    val s = union(s1, s2)
    assert(contains(s, 1), "Union (1),(2) contains 1")
    assert(contains(s, 2), "Union (1),(2) contains 2")
    assert(!contains(s, 3), "Union (1),(2) does not contain 3")
  }

  test("intersect contains all common elements of all sets") {
    val i = intersect(s12, s23)
    assert(!contains(i, 1), "Intersect (1,2),(2,3) does not contain 1")
    assert(contains(i, 2), "Intersect (1,2),(2,3) contains 2")
    assert(!contains(i, 3), "Intersect (1,2),(2,3) does not contain 3")
  }

  test("diff elements from the first set not in the second set") {
    val d = diff(s12, s23)
    assert(contains(d, 1), "Diff (1,2),(2,3) contains 1")
    assert(!contains(d, 2), "Diff (1,2),(2,3) does not contain 2")
    assert(!contains(d, 3), "Diff (1,2),(2,3) does not contains 3")
  }

  test("filter contains common elements of set and filter") {
    val f = filter(s1, t => t != 2)
    assert(contains(f, 1), "Filter contains 1")
    assert(!contains(f, 2), "Filter does not contain 2")
  }

  test("forall returns true if all elements of a set satisfy the condition") {
    val allLessThanTwo = forall(s123, t => t < 2)
    val allLessThanFour = forall(s123, t => t < 4)
    assert(!allLessThanTwo)
    assert(allLessThanFour)
  }

  test("exists returns true if any element of a set satisfy the condition") {
    val anyLessThanTwo = exists(s123, t => t < 2)
    val anyGreaterThanFour = exists(s123, t => t > 4)
    assert(anyLessThanTwo)
    assert(!anyGreaterThanFour)
  }

  test("mapped set contains transformed values") {
    val mappedSet = map(s12, t => t * 3)
    assert(!contains(mappedSet, 1))
    assert(!contains(mappedSet, 2))
    assert(contains(mappedSet, 3))
    assert(contains(mappedSet, 6))
  }


}
