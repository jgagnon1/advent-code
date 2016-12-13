import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by jerome on 2016-12-13.
  */
object Day9 extends App {
  require(decompressLength("(3x3)XYZ") == 9)
  require(decompressLength("A(1x5)BC") == 7)
  require(decompressLength("A(2x2)BCD(2x2)EFG") == 11)

  val source = Source.fromFile("resources/day9.txt").getLines()
  val input = source.toSeq.head

  def decompressLength(s: String, acc: Long = 0, v2: Boolean = false): Long =
    decompressL(s.replaceAll("\\s", "").toList, acc, v2)

  // FIXME : Make it @tailrec again !
  def decompressL(xs: List[Char], acc: Long = 0, v2: Boolean = false): Long = {
    xs match {
      case Nil => acc
      case '(' :: rest =>
        val body = rest.takeWhile(_ != ')').mkString
        val Array(n, times) = body.split("x").map(_.toInt)

        val (content, remainder) = rest.drop(body.length+1).splitAt(n)

        val dsize: Long = if (v2) decompressL(content, 0, v2) else content.length

        decompressL(remainder, acc + times*dsize, v2)
      case _ :: rest => decompressL(rest, acc + 1, v2)
    }
  }

  val res1 = decompressLength(input)
  println(s"Answer 1 : $res1") // 110346

  val res2 = decompressLength(input, v2 = true)
  println(s"Answer 1 : $res2") // 10774309173

}
