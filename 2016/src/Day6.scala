import scala.io.Source

/**
  * Created by jerome on 2016-12-12.
  */
object Day6 extends App {

  val source = Source.fromFile("resources/day6.txt").getLines()

  val transposed = source.toArray.map(_.toCharArray).transpose

  val word = transposed.map { row =>
   row.groupBy(identity).maxBy(_._2.length)
  }
    .map(_._1)
    .mkString

  println(s"Answer 1 : $word") // zcreqgiv

  val word2 = transposed.map { row =>
    row.groupBy(identity).minBy(_._2.length)
  }
    .map(_._1)
    .mkString

  println(s"Answer 2 : $word2")
}
