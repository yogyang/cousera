package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4: FunSet = x => x==1 || x==2

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect should pass") {
    new TestSets:
      assert(!contains(intersect(s1, s2), 1), "Empty intersect")
      assert(contains(intersect(s4, s2), 2), "Intersect 2")
      assert(contains(intersect(s4, s1), 1), "Intersect 1")
  }

  test("diff should pass") {
    new TestSets:
      assert(contains(diff(s1, s2), 1), "Diff 1")
      assert(contains(diff(s4, s2), 1), "Diff 1")
      assert(contains(diff(s4, s1), 2), "Diff 2")
  }

  test("exists should pass") {
    new TestSets:
      assert(exists(s1, x => x > 1), false)
      assert(exists(s1, x => x < 1), false)
      assert(exists(s4, x => x < 0), false)
      assert(exists(s4, x => x > 1), true)
  }

  test("map should pass") {
    new TestSets:
      assert(contains(map(s1, x => x + 1), 2))
      assert(contains(map(s1, x => x - 1), 0))
      assert(contains(map(s4, x => x * 2), 4))
      assert(!contains(map(s4, x => x * 2), 5))
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
