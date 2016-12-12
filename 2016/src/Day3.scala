import scala.io.Source

/**
  * Created by jerome on 2016-12-12.
  */
object Day3 extends App {

  val SideRegex = "\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*".r

  val source = Source.fromFile("resources/day3.txt").getLines().toSeq

  val triangles = source.map { line =>
    val side = SideRegex.findFirstMatchIn(line).get // get to fail on parse error
    Triangle(side.group(1).toInt, side.group(2).toInt, side.group(3).toInt)
  }

  val r1 = triangles.count(_.isPossible)
  println(s"Answer 1 : $r1")

  val triangles2 = source.grouped(3).flatMap { triples =>
    val sides = triples.map(SideRegex.findFirstMatchIn(_).get)
    (1 to 3).map { col =>
      Triangle(sides(0).group(col).toInt, sides(1).group(col).toInt, sides(2).group(col).toInt)
    }
  }

  val r2 = triangles2.count(_.isPossible)
  println(s"Answer 2 : $r2")
}

final case class Triangle(a: Int, b: Int, c: Int) {

  def isPossible: Boolean = a + b > c && a + c > b && b + c > a

}