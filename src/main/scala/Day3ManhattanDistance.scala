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

package jpickard.adventofcode

// refactor so path is it's own thing, that has a direction. can create new path with an initial direction
class Day3ManhattanDistance {

  def manhattanDisTo(target: Int) = {
    var currentNum = 1
    var path = new Path(1, new Location(0,0), new Direction)

    while (currentNum < target) {
      currentNum += 1
      path = path.step
    }
    calcManhattanDistance(new Location(0,0), path.curLoc)
  }

  def calcManhattanDistance(loc1: Location, loc2: Location) =
    Math.abs(loc1.i - loc2.i) + Math.abs(loc1.j - loc2.j)
}

class Day3ManhattanGrid (val w: Int, val h: Int) {
  var grid = _buildGrid

  def firstLargerThan(target: Int) = {
    grid = _buildGrid
    var path = new Path(1, new Location(w/2, h/2), new Direction)
    setVal(1, path.curLoc)
    while (getVal(path.curLoc) <= target) {
      path = path.step
      setVal(calcVal(path.curLoc),path.curLoc)
    }
    getVal(path.curLoc)
  }

  private def setVal(v: Int, loc: Location) = grid(loc.i)(loc.j) = v
  private def getVal(loc: Location) = grid(loc.i)(loc.j)

  private def calcVal(loc: Location) =
    grid(loc.i - 1)(loc.j - 1) + grid(loc.i)(loc.j - 1) + grid(loc.i + 1)(loc.j - 1) +
    grid(loc.i - 1)(loc.j + 1) + grid(loc.i)(loc.j + 1) + grid(loc.i + 1)(loc.j + 1) +
    grid(loc.i - 1)(loc.j) + grid(loc.i + 1)(loc.j)

  private def _buildGrid = {
    val g = new Array[Array[Int]](h)
    g.map(x => new Array[Int](w))
  }
}

class Path (val pathNum: Int, var curLoc: Location, val dir: Direction) {
  var pathStepRemain = Math.ceil(pathNum / 2.0).toInt

  def step = {
    pathStepRemain match {
      case 1 => new Path(pathNum + 1, curLoc.step(dir), dir.nextDirection)
      case _ => {
        curLoc = curLoc.step(dir)
        pathStepRemain -= 1
        this
      }
    }
  }
}

class Direction (val dirIdx: Int = 0) {
  private val directionOrder = List(Direction.RIGHT, Direction.UP, Direction.LEFT, Direction.DOWN)

  def direction = directionOrder(dirIdx)

  def nextDirection = dirIdx >= directionOrder.length - 1 match {
    case true => new Direction
    case false => new Direction(dirIdx + 1)
  }
}

object Direction {
  val LEFT = "LEFT"
  val RIGHT = "RIGHT"
  val UP = "UP"
  val DOWN = "DOWN"
}

class Location(val i: Int, val j: Int) {

  def step(direction: Direction): Location =
    direction.direction match {
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

    val gridCalc = new Day3ManhattanGrid(11,11)
    println("first larger than 2 (4 exp): " + gridCalc.firstLargerThan(2))
    println("first larger than 5 (10 exp): " + gridCalc.firstLargerThan(5))
    println("first larger than 57 (59 exp): " + gridCalc.firstLargerThan(57))
    println("first larger than 304 (330 exp): " + gridCalc.firstLargerThan(304))
    println("answer for 361527: " + gridCalc.firstLargerThan(361527))
  }
}
