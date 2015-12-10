package main.scala.com.lms.gow

class Tile(char: String, speed: Int, range: Int, attack: Int, defense: Int)

case object Void extends Tile(".", 0, 0, 0, 0)

case object Arsenal extends Tile("A", 0, 0, 0, 0)

case object Cannon extends Tile("C", 1, 3, 5, 8)

case object SwiftCannon extends Tile("N", 2, 3, 5, 8)

case object Relay extends Tile("R", 1, 0, 0, 1)

case object SwiftRelay extends Tile("E", 2, 0, 0, 1)

case object Infantry extends Tile("I", 1, 2, 4, 6)

case object Cavalry extends Tile("V", 2, 2, 4, 5)

case object Fortress extends Tile("F", 0, 0, 0, 4)

case object MountainPass extends Tile("=", 0, 0, 0, 2)

case object Mountain extends Tile("M", 0, 0, 0, 0)

class Board {

  val attacksPerTurn = 1
  val movesPerTurn = 5
  val terrainWidth = 25
  val terrainHeight = 20

  val terrain =
    scala.io.Source.fromFile("src/main/resources/init.board")
    .mkString
    .filter(_ > ' ')

  val startingUnits =
    scala.io.Source.fromFile("src/main/resources/init.units")
      .mkString
      .filter(_ > ' ')

  def tileFromChar(char: Char) : Tile = {
    Void
  }

  def getTile(x: Int, y: Int): (Tile, Tile) = {
    val coord = x + (y * terrainWidth)
    (tileFromChar(terrain(coord)), tileFromChar(startingUnits(coord)))
  }

  def printCurrentBoard {

    for (tile <- terrain.zipWithIndex) {
      val t = tile._1
      print(s"$t  ")
      if ((tile._2 + 1) % terrainWidth == 0)
        print("\n")
    }

  }

}
