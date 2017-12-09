/*
Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward.
For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1
(the location of the only access port for this memory system) by programs that can only move up, down, left, or right.
They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

Walk length: 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6

Part B:
store the sum of the values in all adjacent squares, including diagonals.

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

 */

package jpickard.misc

class Day3ManhattanDistance {

  def manhattanDisTo(target: Int) = {
    var current = 1
    var curLoc = new Location(0, 0)
    var dirIdx = 0
    var pathNum = 1
    var pathStepRemain = Direction.pathLength(pathNum)
    while (current < target) {
      if (pathStepRemain == 0) {
        dirIdx = Direction.nextDirectionIdx(dirIdx)
        pathNum += 1
        pathStepRemain = Direction.pathLength(pathNum)
      }

      current += 1
      pathStepRemain -= 1
      curLoc = curLoc.step(dirIdx)
    }
    calcManhattanDistance(new Location(0, 0), curLoc)
  }

  def calcManhattanDistance(loc1: Location, loc2: Location) =
    Math.abs(loc1.i - loc2.i) + Math.abs(loc1.j - loc2.j)
}

object Direction {
  val LEFT = "LEFT"
  val RIGHT = "RIGHT"
  val UP = "UP"
  val DOWN = "DOWN"
  private val directionOrder = List(RIGHT, UP, LEFT, DOWN)

  def nextDirectionIdx(curDirIdx: Int) =
    curDirIdx >= directionOrder.length - 1 match {
      case true => 0
      case false => curDirIdx + 1
    }

  def curDirection(directionIdx: Int) = directionOrder(directionIdx)

  // pathNum is the numeric order of current straight run. length of straight run is pathNum/2 rounded up
  def pathLength(pathNum: Int) = Math.ceil(pathNum / 2).toInt
}

class Location(val i: Int, val j: Int) {

  def step(dirIdx: Int): Location = step(Direction.curDirection(dirIdx))

  def step(direction: String): Location =
    direction match {
      case Direction.RIGHT => new Location(i, j + 1)
      case Direction.LEFT  => new Location(i, j - 1)
      case Direction.UP    => new Location(i - 1, j)
      case Direction.DOWN  => new Location(i + 1, j)
    }
}

object Day3ManhattanDistance {
  def main(args: Array[String]): Unit = {
    val disCalc = new Day3ManhattanDistance()
    println("distance to 1 (0 exp): " + disCalc.manhattanDisTo(1))
    println("distance to 12 (3 exp): " + disCalc.manhattanDisTo(12))
    println("distance to 23 (2 exp): " + disCalc.manhattanDisTo(23))
    println("distance to 1024 (31 exp): " + disCalc.manhattanDisTo(1024))

    println("answer for 361527: " + disCalc.manhattanDisTo(361527))
  }
}
