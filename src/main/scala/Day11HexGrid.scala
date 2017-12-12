/*
The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \

You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

For example:

    ne,ne,ne is 3 steps away.
    ne,ne,sw,sw is 0 steps away (back where you started).
    ne,ne,s,s is 2 steps away (se,se).
    se,sw,se,sw,sw is 3 steps away (s,s,sw).

Hexgrid algorithms from: https://www.redblobgames.com/grids/hexagons/
 */

package jpickard.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day11HexGrid {
  def main(args: Array[String]): Unit = {
    val hg = new Day11HexGrid
    println("0,0 to 5, -2 (5 exp):" + hg.distance(new HexLocation(5,-2)))
    println("-1,1 to 5, -2 (6 exp):" + hg.distance(new HexLocation(5,-2), new HexLocation(-1,1)))
    println("1,1 to 5, -2 (4 exp):" + hg.distance(new HexLocation(5,-2), new HexLocation(1,1)))
    println("-3,3 to 5, -2 (8 exp):" + hg.distance(new HexLocation(5,-2), new HexLocation(-3,3)))
    println("3-,3 to 5, -2 (3 exp):" + hg.distance(new HexLocation(5,-2), new HexLocation(3,-3)))

    println("ne,ne,ne ends (3,-3 exp):" + hg.walkPath("ne,ne,ne"))
    println("ne,ne,sw,sw ends (0,0 exp):" + hg.walkPath("ne,ne,sw,sw"))
    println("ne,ne,s,s ends (0,-2 exp):" + hg.walkPath("ne,ne,s,s"))
    println("se,sw,se,sw,sw ends (-3,1 exp):" + hg.walkPath("se,sw,se,sw,sw"))

    val (loc, maxDis) = hg.walkPathFromFile("day11-path.txt")
    println("answer location/distance: " + loc + "/" + hg.distance(loc) + s" max dis ${maxDis}")
  }
}

// anchor is used for determining the max distance of any location away from anchor - dirty, dirty, dirty...
class Day11HexGrid (val anchorLoc: HexLocation = new HexLocation(0,0)) {
  // three coordinates are used to define a location, however the third (s) is implied from q and r and q + r + s = 0
  def distance(a: HexLocation, b: HexLocation = new HexLocation(0,0)): Int =
    (Math.abs(a.q - b.q) + Math.abs(a.q + a.r - b.q - b.r) + Math.abs(a.r - b.r)) / 2

  // for simplicity we're going to assume starting at HexLocation(0,0) - I know, lazy
  def walkPathFromFile(filename: String) = {
    walkPath(Source.fromResource(filename).getLines.toList.mkString(","))
  }

  // assume a comma-delimited string
  def walkPath(path: String): (HexLocation, Int) = walkPath(path.split(","))

  @tailrec
  final def walkPath(path: Seq[String], loc: HexLocation = new HexLocation(0,0), maxDis: Int = 0): (HexLocation, Int) = {
    path.isEmpty match {
      case true => (loc, maxDis)
      case false =>
        val newLoc = loc.step(path.head)
        val newDis = distance(anchorLoc, newLoc)

        walkPath(path.tail, newLoc, if (maxDis > newDis) maxDis else newDis)
    }
  }
}

class HexDirection (val dirIdx: Int = 0) {
  def this(dirStr: String) = this(HexDirection.directionOrderMap(dirStr.toUpperCase()))

  def direction = HexDirection.directionOrder(dirIdx)

  def nextDirection = dirIdx >= HexDirection.directionOrder.length - 1 match {
    case true => new Direction
    case false => new Direction(dirIdx + 1)
  }
}

object HexDirection {
  val N = "N"
  val NE = "NE"
  val SE = "SE"
  val S = "S"
  val SW = "SW"
  val NW = "NW"

  val directionOrder = List(N, NE, SE, S, SW, NW)
  val directionOrderMap = directionOrder.zipWithIndex.map{ case(e,i) => e -> i }.toMap
  val validDirections: Set[String] = Set(N,NE,SE,S,SW,NW)
}

class HexLocation(val q: Int, val r: Int) {

  override def toString = s"($q,$r)"

  def step(direction: String): HexLocation = step(new HexDirection(direction))

  def step(direction: HexDirection): HexLocation =
    direction.direction match {
      case HexDirection.N  => new HexLocation(q + 1, r)
      case HexDirection.NE => new HexLocation(q + 1, r - 1)
      case HexDirection.SE => new HexLocation(q, r - 1)
      case HexDirection.S  => new HexLocation(q - 1, r)
      case HexDirection.SW => new HexLocation(q - 1, r + 1)
      case HexDirection.NW => new HexLocation(q , r + 1)
    }
}