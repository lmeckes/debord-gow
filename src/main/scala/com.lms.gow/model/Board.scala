package com.lms.gow.model

abstract case class Tile(char: Char, speed: Int, range: Int, attack: Int,
                         defense: Int, isUnit: Boolean, isCom: Boolean, isPlayer1: Boolean)
object VoidTile extends Tile('.', 0, 0, 0, 0, false, false, false)
object Fortress extends Tile('F', 0, 0, 0, 4, false, false, false)
object Mountain extends Tile('M', 0, 0, 0, 0, false, false, false)
object MountainPass extends Tile('=', 0, 0, 0, 2, false, false, false)
object RedArsenal extends Tile('A', 0, 0, 0, 0, true, true, false)
object RedRelay extends Tile('R', 1, 0, 0, 1, true, true, false)
object RedSwiftRelay extends Tile('E', 2, 0, 0, 1, true, true, false)
object RedCannon extends Tile('C', 1, 3, 5, 8, true, false, false)
object RedSwiftCannon extends Tile('N', 2, 3, 5, 8, true, false, false)
object RedInfantry extends Tile('I', 1, 2, 4, 6, true, false, false)
object RedCavalry extends Tile('V', 2, 2, 4, 5, true, false, false)
object BlueArsenal extends Tile('a', 0, 0, 0, 0, true, true, true)
object BlueRelay extends Tile('r', 1, 0, 0, 1, true, true, true)
object BlueSwiftRelay extends Tile('e', 2, 0, 0, 1, true, true, true)
object BlueCannon extends Tile('c', 1, 3, 5, 8, true, false, true)
object BlueSwiftCannon extends Tile('n', 2, 3, 5, 8, true, false, true)
object BlueInfantry extends Tile('i', 1, 2, 4, 6, true, false, true)
object BlueCavalry extends Tile('v', 2, 2, 4, 5, true, false, true)

abstract case class Direction(y: Int, x: Int)
object N extends Direction(-1, 0)
object NE extends Direction(-1, 1)
object E extends Direction(0, 1)
object SE extends Direction(1, 1)
object S extends Direction(1, 0)
object SW extends Direction(1, -1)
object W extends Direction(0, -1)
object NW extends Direction(-1, -1)

import scala.collection._
import com.lms.gow.model.Util._

object Rules {
  val terrainWidth = 25
  val terrainHeight = 20
  val totalTiles = terrainWidth * terrainHeight
  val movesPerTurn = 5
  val attacksPerTurn = 1
  val terrainTilesRepository = Seq(Fortress, Mountain, MountainPass)
  val unitTilesRepository = Seq(BlueCannon, BlueSwiftCannon, BlueRelay, BlueSwiftRelay, BlueInfantry, BlueCavalry, BlueArsenal,
    RedCannon, RedSwiftCannon, RedRelay, RedSwiftRelay, RedInfantry, RedCavalry, RedArsenal)
  val startingTerrain = loadTilesFromFile("src/main/resources/init.board", terrainTilesRepository)
  val startingUnits = loadTilesFromFile("src/main/resources/init.units", unitTilesRepository)
  val directions = Seq(N, NE, E, SE, S, SW, W, NW)
}

object Util {

  def indexFromCoordinates(x: Int, y: Int) = (x + (y * Rules.terrainWidth))

  //def coordinatesFromIndex(x: Int): (Int, Int) = (x % Rules.terrainWidth, x / Rules.terrainWidth)

  def loadTilesFromFile(file: String, tileRepository: Seq[Tile]): Seq[Tile] = {
    io.Source.fromFile(file)
      .mkString
      .filter(_ > ' ')
      .map((tileRepository.map(_.char) zip tileRepository)
        .toMap
        .get(_)
        .getOrElse(VoidTile))
  }

}

class Board {

  val terrainLayer = Rules.startingTerrain
  var unitLayer = Rules.startingUnits.zipWithIndex.map(_.swap).toMap
  val comLayerRed : Seq[mutable.Set[Direction]] = Seq.fill(Rules.totalTiles)(mutable.Set[Direction]())
  val comLayerBlue : Seq[mutable.Set[Direction]] = Seq.fill(Rules.totalTiles)(mutable.Set[Direction]())

  var player1Turn = true
  var leftMoves = Rules.movesPerTurn

  refreshComLayer

  def refreshComLayer = {

    comLayerRed.foreach(d => d.clear)
    comLayerBlue.foreach(d => d.clear)

    unitLayer.filter(u => u._2.eq(RedArsenal) || u._2.eq(BlueArsenal))
      .map(propagateCom(_))

    def propagateCom(source: (Int, Tile)): Unit = {

      Rules.directions.foreach(dir => {

        var doRun = true
        var index = source._1

        while (doRun) {
          index += dir.x + (dir.y * Rules.terrainWidth)
          if (index % Rules.terrainWidth < 1 || !(0 until Rules.totalTiles).contains(index)
            || !shouldPropagateCom(source._2.isPlayer1, terrainLayer(index), unitLayer(index))) {
            doRun = false
          } else {
            if (source._2.isPlayer1) {
              comLayerBlue(index) += dir
            } else {
              comLayerRed(index) += dir
            }
          }
        }
      })

      def shouldPropagateCom(player1: Boolean, terrain: Tile, unit: Tile): Boolean = {
        if (terrain == Mountain ||
          (Rules.unitTilesRepository.contains(unit)
            && unit.isPlayer1 != player1))
          false
        else
          true
      }

    }

  }

  def getUnitTile(x: Int, y: Int) = unitLayer(indexFromCoordinates(x, y))
  def getTerrainTile(x: Int, y: Int) = terrainLayer(indexFromCoordinates(x, y))
  def getComTile(x: Int, y: Int) = (comLayerRed(indexFromCoordinates(x, y)), comLayerBlue(indexFromCoordinates(x, y)))

  def move(x: Int, y: Int, newX: Int, newY: Int): Boolean = {

    val srcCoord = indexFromCoordinates(x, y)
    val dstCoord = indexFromCoordinates(newX, newY)
    val unit = unitLayer.get(srcCoord).getOrElse(VoidTile)
    val dstUnit = unitLayer.get(dstCoord).getOrElse(VoidTile)
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
      unitLayer += srcCoord -> VoidTile
      unitLayer += dstCoord -> unit

      refreshComLayer

      return true
    } else {
      return false
    }
  }


  def dump {
    for (tile <- unitLayer) {
      val t = tile._2
      print(s"$t  ")
      if ((tile._1 + 1) % Rules.terrainWidth == 0)
        print("\n")
    }
  }

}
