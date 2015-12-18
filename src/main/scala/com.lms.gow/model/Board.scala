package com.lms.gow.model

abstract case class Tile(char: Char, speed: Int, range: Int, attack: Int, defense: Int, isUnit: Boolean, isCom: Boolean, isPlayer1: Boolean)

object VoidTile             extends Tile('.', 0, 0, 0, 0, false, false, false)
object Fortress             extends Tile('F', 0, 0, 0, 4, false, false, false)
object Mountain             extends Tile('M', 0, 0, 0, 0, false, false, false)
object MountainPass         extends Tile('=', 0, 0, 0, 2, false, false, false)
object RedArsenal           extends Tile('A', 0, 0, 0, 0, true, true, true)
object RedCannon            extends Tile('C', 1, 3, 5, 8, true, false, true)
object RedSwiftCannon       extends Tile('N', 2, 3, 5, 8, true, false, true)
object RedRelay             extends Tile('R', 1, 0, 0, 1, true, true, true)
object RedSwiftRelay        extends Tile('E', 2, 0, 0, 1, true, true, true)
object RedInfantry          extends Tile('I', 1, 2, 4, 6, true, false, true)
object RedCavalry           extends Tile('V', 2, 2, 4, 5, true, false, true)
object BlueArsenal          extends Tile('a', 0, 0, 0, 0, true, true, false)
object BlueCannon           extends Tile('c', 1, 3, 5, 8, true, false, false)
object BlueSwiftCannon      extends Tile('n', 2, 3, 5, 8, true, false, false)
object BlueRelay            extends Tile('r', 1, 0, 0, 1, true, true, false)
object BlueSwiftRelay       extends Tile('e', 2, 0, 0, 1, true, true, false)
object BlueInfantry         extends Tile('i', 1, 2, 4, 6, true, false, false)
object BlueCavalry          extends Tile('v', 2, 2, 4, 5, true, false, false)

class Board {

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
      .toUpperCase
      .map((terrainTiles.map(_.char) zip terrainTiles).toMap.get(_).getOrElse(VoidTile))

  val startingUnits =
    scala.io.Source.fromFile("src/main/resources/init.units")
      .mkString
      .filter(_ > ' ')
      .map(char => (unitTiles.map(_.char) zip unitTiles).toMap.get(char).getOrElse(VoidTile))

  var currentUnits = startingUnits//.zipWithIndex.map(if (_._1 >= 250) )

  def getTile(x: Int, y: Int): (Tile, Tile) = {
    val coord = (x + (y * terrainWidth))
    (startingTerrain(coord), currentUnits(coord))
  }

  def dump {
    for (tile <- currentUnits.zipWithIndex) {
      val t = tile._1.char
      print(s"$t  ")
      if ((tile._2 + 1) % terrainWidth == 0)
        print("\n")
    }
  }

}
