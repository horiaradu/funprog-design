package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("Block((1, 1), (1, 1))) is standing") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).isStanding)
    }
  }

  test("Block((1, 1), (2, 1))) isn't standing") {
    new Level1 {
      assert(!Block(Pos(1, 1), Pos(2, 1)).isStanding)
    }
  }

  test("Block((0, 0), (0, 1)) is legal") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 1)).isLegal)
    }
  }

  test("Block((-1, 0), (0, 0)) isn't legal") {
    new Level1 {
      assert(!Block(Pos(-1, 0), Pos(0, 0)).isLegal)
    }
  }

  test("Block((5, 8), (5,9)) isn't legal") {
    new Level1 {
      assert(!Block(Pos(5, 8), Pos(5, 9)).isLegal)
    }
  }

  test("block's neighbors") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).neighbors ==
        List(
          (Block(Pos(0, -2), Pos(0, -1)), Left),
          (Block(Pos(0, 1), Pos(0, 2)), Right),
          (Block(Pos(-2, 0), Pos(-1, 0)), Up),
          (Block(Pos(1, 0), Pos(2, 0)), Down)
        )
      )
    }
  }

  test("block's legal neighbors") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).legalNeighbors ==
        List(
          (Block(Pos(0, 1), Pos(0, 2)), Right),
          (Block(Pos(1, 0), Pos(2, 0)), Down)
        )
      )
    }
  }

  test("neighbors with history") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet ===
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        )
      )
    }
  }

  test("new neighbors only") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      ) ===
        Set(
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream
      )
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
