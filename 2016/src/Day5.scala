import java.security.MessageDigest

import scala.annotation.tailrec

/**
  * Created by jerome on 2016-12-12.
  */
object Day5 extends App {

  val digest = MessageDigest.getInstance("MD5")

  def md5(text: String) = {
    digest.digest(text.getBytes)
  }

  val input = "ojvtpuvg"

  // Tail-rec because it's fun! :D

  @tailrec
  def tryCode(acc: Seq[Char], idx: Int = 0): Seq[Char] = {
    if (acc.size == 8) {
      acc
    } else {
      val hash = md5(s"$input$idx")
      // Optimization : Speed up by not constructing String every iteration
      if (hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0) {
        val str = hash.map("%02x".format(_)).mkString
        println(s"Found next, passwd size : ${acc.size + 1}")
        tryCode(acc :+ str(5), idx + 1)
      } else {
        tryCode(acc, idx + 1)
      }
    }
  }

  val res = tryCode(Seq.empty)
  println(s"Answer 1: ${res.mkString}") // 4543c154

  @tailrec
  def tryCode2(acc: Seq[Option[Char]], idx: Int = 0): Seq[Option[Char]] = {
    if (acc.flatten.size == 8) {
      acc
    } else {
      val hash = md5(s"$input$idx")
      // Optimization : Speed up by not constructing String every iteration
      if (hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0) {
        val str = hash.map("%02x".format(_)).mkString
        val pos = str(5).asDigit
        val n = str(6)

        // Ignore invalid position
        if (pos < acc.size && acc(pos).isEmpty) {
          println(s"Found next, passwd size : ${acc.flatten.size + 1}")
          tryCode2(acc.updated(pos, Some(n)), idx + 1)
        } else tryCode2(acc, idx + 1)
      } else {
        tryCode2(acc, idx + 1)
      }
    }
  }

  val res2 = tryCode2(Seq.fill(8)(None))
  println(s"Answer 2: ${res2.flatten.mkString}") // 1050cbbd

}
