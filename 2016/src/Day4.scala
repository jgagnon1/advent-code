import scala.io.Source

/**
  * Created by jerome on 2016-12-12.
  */
object Day4 extends App {
  require(Room("notarealroom", 404, "oarel").isReal)
  require(!Room("totallyrealroom", 200, "decoy").isReal)

  val source = Source.fromFile("resources/day4.txt").getLines()

  val rooms = source.map { line =>
    val pivot = line.lastIndexOf("-")
    val (name, rest) = line.splitAt(pivot)
    val Array(sectorId, checkSum) = rest.tail.split("\\[", 2)
    Room(name, sectorId.toInt, checkSum.init)
  }

  val realRooms = rooms.filter(_.isReal).toSeq

  val s1 = realRooms
    .map(_.sectorId)
    .sum

  println(s"Answer 1: $s1") // 185371

  val s2 = realRooms
    .map(r => r.copy(name = r.realName))
    .find(_.name contains "north")

  println(s"Answer 2 : $s2") // 984
}

final case class Room(name: String, sectorId: Int, checksum: String) {

  val Alphabet: Seq[Char] = 'a' to 'z'

  def isReal: Boolean = {
    val top = name
      .replaceAll("-", "")
      .groupBy(identity)
      .toSeq
      .sortBy(t => (-t._2.length, t._1))
      .take(5).map(_._1)

    top.forall(checksum.toCharArray.contains)
  }

  def realName: String = {
    (0 until sectorId).foldLeft(name.replaceAll("-", " ")) { case (state, _) => shift(state) }
  }

  private def shift(in: String) = {
    in.map {
      case ' ' => ' '
      case c => Alphabet((Alphabet.indexOf(c) + 1) % Alphabet.size)
    }
  }

}
