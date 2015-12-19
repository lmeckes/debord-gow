package com.lms.gow.model

abstract case class Tile(char: Char, speed: Int, range: Int, attack: Int,
                         defense: Int, isUnit: Boolean, isCom: Boolean, isPlayer1: Boolean)

object VoidTile             extends Tile('.', 0, 0, 0, 0, false, false, false)
object Fortress             extends Tile('F', 0, 0, 0, 4, false, false, false)
object Mountain             extends Tile('M', 0, 0, 0, 0, false, false, false)
object MountainPass         extends Tile('=', 0, 0, 0, 2, false, false, false)
object RedArsenal           extends Tile('A', 0, 0, 0, 0, true, true, false)
object RedCannon            extends Tile('C', 1, 3, 5, 8, true, false, false)
object RedSwiftCannon       extends Tile('N', 2, 3, 5, 8, true, false, false)
object RedRelay             extends Tile('R', 1, 0, 0, 1, true, true, false)
object RedSwiftRelay        extends Tile('E', 2, 0, 0, 1, true, true, false)
object RedInfantry          extends Tile('I', 1, 2, 4, 6, true, false, false)
object RedCavalry           extends Tile('V', 2, 2, 4, 5, true, false, false)
object BlueArsenal          extends Tile('a', 0, 0, 0, 0, true, true, true)
object BlueCannon           extends Tile('c', 1, 3, 5, 8, true, false, true)
object BlueSwiftCannon      extends Tile('n', 2, 3, 5, 8, true, false, true)
object BlueRelay            extends Tile('r', 1, 0, 0, 1, true, true, true)
object BlueSwiftRelay       extends Tile('e', 2, 0, 0, 1, true, true, true)
object BlueInfantry         extends Tile('i', 1, 2, 4, 6, true, false, true)
object BlueCavalry          extends Tile('v', 2, 2, 4, 5, true, false, true)

class Board {

  object Config {
    val terrainTiles = Seq(Fortress, Mountain, MountainPass)
    val unitTiles = Seq(BlueCannon, BlueSwiftCannon, BlueRelay, BlueSwiftRelay, BlueInfantry, BlueCavalry, BlueArsenal,
      RedCannon, RedSwiftCannon, RedRelay, RedSwiftRelay, RedInfantry, RedCavalry, RedArsenal)
    val attacksPerTurn = 1
    val movesPerTurn = 5
    val terrainWidth = 25
    val terrainHeight = 20
    val startingTerrain =
      scala.io.Source.fromFile("src/main/resources/init.board")
        .mkString
        .filter(_ > ' ')
        .map((terrainTiles.map(_.char) zip terrainTiles).toMap.get(_).getOrElse(VoidTile))
    val startingUnits =
      scala.io.Source.fromFile("src/main/resources/init.units")
        .mkString
        .filter(_ > ' ')
        .map((unitTiles.map(_.char) zip unitTiles).toMap.get(_).getOrElse(VoidTile))
  }


  // Current Game
  var player1Turn = true
  var leftMoves = Config.movesPerTurn
  var currentUnits = Config.startingUnits.zipWithIndex.map(_.swap).toMap

  def move(x: Int, y: Int, newX: Int, newY: Int): Boolean = {

    val srcCoord = (x + (y * Config.terrainWidth))
    val dstCoord = (newX + (newY * Config.terrainWidth))
    val unit = currentUnits.get(srcCoord).getOrElse(VoidTile)
    val dst = currentUnits.get(dstCoord).getOrElse(VoidTile)

    if (unit.isPlayer1 == player1Turn && dst.equals(VoidTile) && leftMoves > 0) {
      leftMoves = leftMoves - 1
      if (leftMoves == 0) {
        leftMoves = Config.movesPerTurn
        player1Turn = !player1Turn
      }
      currentUnits += srcCoord -> VoidTile
      currentUnits += dstCoord -> unit
      return true
    } else {
      return false
    }
  }

  def getTile(x: Int, y: Int): (Tile, Tile) = {
    val coord = (x + (y * Config.terrainWidth))
    (Config.startingTerrain(coord), currentUnits(coord))
  }

  def dump {
    for (tile <- currentUnits) {
      val t = tile._2
      print(s"$t  ")
      if ((tile._1 + 1) % Config.terrainWidth == 0)
        print("\n")
    }
  }

}
