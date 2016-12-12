import Direction._

import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by jerome on 2016-12-03.
  */
object Day1 extends App {

  val source = Source.fromFile("resources/day1.txt").getLines().toSeq
  val instructions = source.head.split(",").map(_.trim.splitAt(1))

  val startPos = Position(0, 0, North)

  val finalPos = instructions.foldLeft(startPos) { case (pos, (turn, steps)) =>
    val newPos = turn match {
      case "L" => pos.turnLeft()
      case "R" => pos.turnRight()
      case _ =>
        println("Unknown directions.")
        pos
    }

    newPos.walk(steps.toInt)
  }

  val distance = startPos.distanceTo(finalPos)
  println(s"Answer 1 : $distance")

  val allPos: Seq[Position] = instructions.foldLeft(Seq(startPos)) { case (pos, (turn, steps)) =>
    turn match {
      case "L" => pos.init ++ 1.to(steps.toInt).scanLeft(pos.last.turnLeft()) { case (a, _) => a.walk(1) }
      case "R" => pos.init ++ 1.to(steps.toInt).scanLeft(pos.last.turnRight()) { case (a, _) => a.walk(1) }
      case _ =>
        println("Unknown directions.")
        Seq.empty[Position]
    }
  }

  @tailrec
  def findFirstDuplicate(list: List[Position]): Option[Position] = list match {
    case head :: tail if tail.exists(p => p.x == head.x && p.y == head.y) =>
      Some(head)
    case head :: tail =>
      findFirstDuplicate(tail)
  }

  val firstDup: Option[Position] = findFirstDuplicate(allPos.toList)
  println(s"Answer 2: ${firstDup.map(_.distanceTo(startPos))}")
}

final case class Position(x: Int, y: Int, dir: Direction) {

  def distanceTo(position: Position): Long = (position.x - x).abs + (position.y - y).abs

  def turnRight(): Position =
    this.dir match {
      case North => this.copy(dir = East)
      case West => this.copy(dir = North)
      case South => this.copy(dir = West)
      case East => this.copy(dir = South)
    }

  def turnLeft(): Position =
    this.dir match {
      case North => this.copy(dir = West)
      case West => this.copy(dir = South)
      case South => this.copy(dir = East)
      case East => this.copy(dir = North)
    }

  def walk(steps: Int): Position =
    this.dir match {
      case North => this.copy(y = y + steps)
      case East => this.copy(x = x + steps)
      case South => this.copy(y = y - steps)
      case West => this.copy(x = x - steps)
    }

}

sealed trait Direction

object Direction {

  case object North extends Direction

  case object West extends Direction

  case object South extends Direction

  case object East extends Direction

}
