package main.scala.com.lms.gow

case class Tile(char: Char, speed: Int, range: Int, attack: Int, defense: Int)

object VoidTile     extends Tile('.', 0, 0, 0, 0)
object Arsenal      extends Tile('A', 0, 0, 0, 0)
object Cannon       extends Tile('C', 1, 3, 5, 8)
object SwiftCannon  extends Tile('N', 2, 3, 5, 8)
object Relay        extends Tile('R', 1, 0, 0, 1)
object SwiftRelay   extends Tile('E', 2, 0, 0, 1)
object Infantry     extends Tile('I', 1, 2, 4, 6)
object Cavalry      extends Tile('V', 2, 2, 4, 5)
object Fortress     extends Tile('F', 0, 0, 0, 4)
object MountainPass extends Tile('=', 0, 0, 0, 2)
object Mountain     extends Tile('M', 0, 0, 0, 0)

class Board {

  val terrainTiles = Seq(Fortress, Arsenal, Mountain, MountainPass)
  val unitTiles = Seq(Cannon, SwiftCannon, Relay, SwiftRelay, Infantry, Cavalry)
  val attacksPerTurn = 1
  val movesPerTurn = 5
  val terrainWidth = 25
  val terrainHeight = 20

  val startingTerrain =
    scala.io.Source.fromFile("src/main/resources/init.board")
      .mkString
      .filter(_ > ' ')
      .toUpperCase
      .map((terrainTiles.map(_.char) zip terrainTiles).toMap.get(_).getOrElse(VoidTile))

  val startingUnits =
    scala.io.Source.fromFile("src/main/resources/init.units")
      .mkString
      .filter(_ > ' ')
      .toUpperCase
      .map((unitTiles.map(_.char) zip unitTiles).toMap.get(_).getOrElse(VoidTile))

  var currentUnits = startingUnits

  def getTile(x: Int, y: Int): (Tile, Tile) = {
    val coord = x + (y * terrainWidth)
    (startingTerrain(coord), startingUnits(coord))
  }

  def printCurrentBoard {

    for (tile <- currentUnits.zipWithIndex) {
      val t = tile._1.char
      print(s"$t  ")
      if ((tile._2 + 1) % terrainWidth == 0)
        print("\n")
    }

  }

}
