import scala.io.Source

/**
  * Created by jerome on 2016-12-05.
  */
object Day2 extends App {

  val source = Source.fromFile("resources/day2.txt").getLines()
  // sample 1985 : val source = Seq("ULL", "RRDDD", "LURDL", "UUUUD")

  val lines = source.map(_.toCharArray).toSeq

  def findCode(keypad: Seq[Seq[Option[String]]], initial: Point): String = {
    val codes = lines.foldLeft(List(Point(1, 1))) { case (xs@prev :: _, line) =>
      val next = line.foldLeft(prev) {
        case (p@Point(x, y), 'U') if keypad.isDefinedAt(y - 1) && keypad(y - 1)(x).isDefined => p.copy(y = y - 1)
        case (p@Point(x, y), 'D') if keypad.isDefinedAt(y + 1) && keypad(y + 1)(x).isDefined => p.copy(y = y + 1)
        case (p@Point(x, y), 'R') if keypad(y).isDefinedAt(x + 1) && keypad(y)(x + 1).isDefined => p.copy(x = x + 1)
        case (p@Point(x, y), 'L') if keypad(y).isDefinedAt(x - 1) && keypad(y)(x - 1).isDefined => p.copy(x = x - 1)
        case (p, m) => p
      }
      next :: xs
    }

    codes.init.reverse.flatMap(p => keypad(p.y)(p.x)).mkString("")
  }

  val keypad1 = Seq(
    Seq("1", "2", "3"),
    Seq("4", "5", "6"),
    Seq("7", "8", "9")
  ).map(_.map(Some(_)))

  val code1 = findCode(keypad1, Point(1, 1))
  println(s"Answer 1 : $code1") // 36629

  val keypad2 = Seq(
    Seq("_", "_", "1", "_", "_"),
    Seq("_", "2", "3", "4", "_"),
    Seq("5", "6", "7", "8", "9"),
    Seq("_", "A", "B", "C", "_"),
    Seq("_", "_", "D", "_", "_")
  ).map { line =>
    line.map {
      case "_" => None
      case b => Some(b)
    }
  }

  val code2 = findCode(keypad2, Point(0, 2))
  println(s"Answer 2 : $code2") // 99C3D
}

final case class Point(x: Int, y: Int)

