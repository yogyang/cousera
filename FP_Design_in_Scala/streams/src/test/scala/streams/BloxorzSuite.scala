package streams

import Bloxorz.*
import scala.annotation.tailrec

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
        case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
    }

  trait Level1 extends SolutionChecker:
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)


  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }

  test("neighborsWithHistory") {
    new Level1:
      import Move.*
      val r = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left, Up))
      assert(r.toSet == Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),(Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
  }

  test("newNeighborsOnly") {
    new Level1:
      import Move.*
      val r = newNeighborsOnly(Set(
      (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
      (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).to(LazyList),
      Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1))))

      assert(r.toSet == Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
  }

  test("fromStartPath") {
    new Level1:
      val r = pathsFromStart
      r.takeWhile(p => p._2.length < 4).foreach(s => println(s"to ${s._1} with moves ${s._2}"))

      var r2 = solution
      println(r2.reverse.mkString(" "))
  }

  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }

  test("stream basic") {
    def fromN(n: Int): LazyList[Int] = {
      n #:: fromN(n+1)
    }
    val from3 = fromN(3)
    // from3.filter(_ <= 5).foreach(println(_))
    from3.takeWhile( _ <= 5).foreach(println(_))


    (0 to 15).collect {
      case n if n % 3 == 0 => 
        println(s"in L1 ${n}")
        n
    }.collect {
      case n if n % 5 == 0 => 
        println(s"in L2 ${n}")
        n
    }

    15.toString().toCharArray().map(_.toInt)
  }

  test("n") {
    @tailrec
    def digitalRoot(n: Int): Int = {
      require(n > 0)
      
      @tailrec
      def inner(n: Int, sum: Int): Int = {
        if (n < 10) sum + n
        else inner(n/10, sum + n % 10)
      }

      val s = inner(n, 0)
      if (s > 10) digitalRoot(s) else s
    }

    println(digitalRoot(Int.MaxValue))
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
