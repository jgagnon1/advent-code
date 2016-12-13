import scala.io.Source

/**
  * Created by jerome on 2016-12-13.
  */
object Day8 extends App {

  val source = Source.fromFile("resources/day8.txt").getLines()
  val intialGrid = Array.fill(6) { Array.fill(50)(".") }

//  val source = Seq("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1")
//  val grid = Array.fill(3) { Array.fill(7)(".") }

  val instructions = source.map(_.split(" "))

  val finalGrid = instructions.foldLeft(intialGrid) {

    case (grid, Array("rect", dim)) =>
      val Array(w, h) = dim.split("x").map(_.toInt)
      (0 until h) foreach { x =>
        (0 until w) foreach { y =>
          grid(x)(y) = "#"
        }
      }
      grid

    case (grid, Array("rotate", "row", coord, "by", n)) =>
      val y = coord.split("=").last.toInt

      grid(y) = rotate(grid(y), n.toInt)
      grid

    case (grid, Array("rotate", "column", coord, "by", n)) =>
      val x = coord.split("=").last.toInt

      val col = grid.map(row => row(x))
      val shifted = rotate(col, n.toInt)

      grid.zipWithIndex.foreach { case (row, idx) =>
        row(x) = shifted(idx)
        row
      }
      grid
  }

  finalGrid.foreach { row =>
    println(row.mkString)
  }

  val res1 = finalGrid.flatten.count(_ == "#")
  println(s"Answer 1  $res1")

  def rotate(l: Array[String], n: Int): Array[String] = {
    val shiftStream = Stream.continually(l.reverse).flatten.sliding(l.length).map(_.reverse)
    shiftStream.take(n+1).toSeq.last.toArray
  }

}
