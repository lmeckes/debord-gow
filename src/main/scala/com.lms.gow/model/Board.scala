package com.lms.gow.model

abstract case class Tile(char: Char, speed: Int, range: Int, attack: Int,
                         defense: Int, isUnit: Boolean, isCom: Boolean, isBlue: Boolean)
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
  val comLayerRed = Seq.fill(Rules.totalTiles)(mutable.Set[Direction]())
  val comLayerBlue = Seq.fill(Rules.totalTiles)(mutable.Set[Direction]())

  var player1Turn = true
  var leftMoves = Rules.movesPerTurn

  refreshComLayer

  def refreshComLayer = {

    comLayerRed.foreach(_.clear)
    comLayerBlue.foreach(_.clear)
    unitLayer.filter(u => u._2.eq(RedArsenal) || u._2.eq(BlueArsenal))
      .map(propagateCom(_))

    def propagateCom(source: (Int, Tile)): Unit = {

      val index = source._1
      val isBlue = source._2.isBlue

      if (isBlue)
        comLayerBlue(index) ++= Rules.directions
      else
        comLayerRed(index) ++= Rules.directions

      Rules.directions.foreach(dir => {
        var pos = index
        var doRun = true
        while (doRun) {
          pos += dir.x + (dir.y * Rules.terrainWidth)
          if (shouldBreakCom(pos, isBlue)) {
            doRun = false
          } else {
            if (isBlue)
              comLayerBlue(pos) += dir
            else
              comLayerRed(pos) += dir

            val shouldRelay =
              (Seq(BlueRelay, BlueSwiftRelay, RedRelay, RedSwiftRelay).contains(unitLayer(pos))
                && unitLayer(pos).isBlue.equals(isBlue)
                && comLayerBlue(pos).size < Rules.directions.size && comLayerRed(pos).size < Rules.directions.size)
            if (shouldRelay)
              propagateCom(pos, unitLayer(pos))

          }
        }
      })

      def shouldBreakCom(pos : Int, isBlue: Boolean): Boolean = {
        if (pos % Rules.terrainWidth < 1 || !(0 until Rules.totalTiles).contains(pos))
          true
        else {
          val terrain = terrainLayer(pos)
          val unit = unitLayer(pos)
          if (terrain == Mountain ||
          (Rules.unitTilesRepository.contains(unit)
            && unit.isBlue != isBlue))
          true
        else
          false
        }
      }
    }

  }

  def getUnitTile(x: Int, y: Int) = unitLayer(indexFromCoordinates(x, y))
  def getTerrainTile(x: Int, y: Int) = terrainLayer(indexFromCoordinates(x, y))
  def getComTile(x: Int, y: Int) = (comLayerRed(indexFromCoordinates(x, y)), comLayerBlue(indexFromCoordinates(x, y)))
  def hasCom(x: Int, y:Int) = {
    val unit = unitLayer(indexFromCoordinates(x, y))
    if (unit.eq(VoidTile))
      false
    else if (unit.isBlue)
      getComTile(x, y)._2.size > 0
    else
      getComTile(x, y)._1.size > 0
  }

  def move(x: Int, y: Int, newX: Int, newY: Int): Boolean = {

    val srcCoord = indexFromCoordinates(x, y)
    val dstCoord = indexFromCoordinates(newX, newY)
    val unit = unitLayer.get(srcCoord).getOrElse(VoidTile)
    val dstUnit = unitLayer.get(dstCoord).getOrElse(VoidTile)
    val dstTerrain = Rules.startingTerrain(dstCoord)

    if (unit.isBlue == player1Turn
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
