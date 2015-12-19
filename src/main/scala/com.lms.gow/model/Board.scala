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

import Util._

object Rules {

  val attacksPerTurn = 1
  val movesPerTurn = 5
  val terrainWidth = 25
  val terrainHeight = 20

  val terrainTiles = Seq(Fortress, Mountain, MountainPass)
  val unitTiles = Seq(BlueCannon, BlueSwiftCannon, BlueRelay, BlueSwiftRelay, BlueInfantry, BlueCavalry, BlueArsenal,
    RedCannon, RedSwiftCannon, RedRelay, RedSwiftRelay, RedInfantry, RedCavalry, RedArsenal)

  val startingTerrain = loadTiles("src/main/resources/init.board", terrainTiles)
  val startingUnits = loadTiles("src/main/resources/init.units", unitTiles)
}

object Util {

  def indexFromCoordinates(x: Int, y: Int) = (x + (y * Rules.terrainWidth))

  def loadTiles(file: String, tileRef: Seq[Tile]): Seq[Tile] = {
    scala.io.Source.fromFile(file)
      .mkString
      .filter(_ > ' ')
      .map((tileRef.map(_.char) zip tileRef)
        .toMap
        .get(_)
        .getOrElse(VoidTile))
  }

}

class Board {

  var player1Turn = true
  var leftMoves = Rules.movesPerTurn
  var currentUnits = Rules.startingUnits.zipWithIndex.map(_.swap).toMap

  def move(x: Int, y: Int, newX: Int, newY: Int): Boolean = {

    val srcCoord = indexFromCoordinates(x, y)
    val dstCoord = indexFromCoordinates(newX, newY)
    val unit = currentUnits.get(srcCoord).getOrElse(VoidTile)
    val dstUnit = currentUnits.get(dstCoord).getOrElse(VoidTile)
    val dstTerrain = Rules.startingTerrain(dstCoord)

    if (unit.isPlayer1 == player1Turn
      && dstUnit.equals(VoidTile)
      && !dstTerrain.equals(Mountain)
      && leftMoves > 0) {
        leftMoves = leftMoves - 1
        if (leftMoves == 0) {
          leftMoves = Rules.movesPerTurn
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
    val coord = indexFromCoordinates(x, y)
    (Rules.startingTerrain(coord), currentUnits(coord))
  }

  def dump {
    for (tile <- currentUnits) {
      val t = tile._2
      print(s"$t  ")
      if ((tile._1 + 1) % Rules.terrainWidth == 0)
        print("\n")
    }
  }

}
