package com.lms.gow.ui

import javafx.beans
import javafx.beans.InvalidationListener
import javafx.event.EventHandler
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.Image
import javafx.scene.input.MouseEvent
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

import com.lms.gow.model._

import scala.collection.mutable
import scala.util.Random

class BoardPane(g: Game) extends Pane {

  def tileSize = 50

  val width = tileSize * Rules.terrainWidth
  val height = tileSize * Rules.terrainHeight

  private def tileImage(t: Tile) = new Image("tiles/" + t.char.toString + ".png")

  val images = (g.terrainLayer ++ g.unitLayer)
    .filterNot(_.eq(VoidTile))
    .map(t => (t.char, tileImage(t))).toMap

  val query = new GameQuery(g)

  var selected: (Int, Int) = _
  var moveOptions: Seq[(Int, Int)] = _
  var selectSquare: Rectangle = _
  var moveSquares: Seq[Rectangle] = _

  def onTileClicked(event: MouseEvent) = {

    val x = (math.floor(event.getSceneX / tileSize)).toInt
    val y = (math.floor(event.getSceneY / tileSize)).toInt

    if (query.canUnitMove(x, y)) {
      selected = (x, y)
      moveOptions = query.getCoordinatesInRange(x, y, 1)
      refreshUxLayer
    } else if (selectSquare != null) {
      g.move(selected._1, selected._2, x, y)
      scanBoardCoordinates(drawTileLayers)
    }

    def refreshUxLayer = {

      if (selectSquare != null)
        getChildren.remove(selectSquare)

      selectSquare = new Rectangle(selected._1 * tileSize, selected._2 * tileSize, tileSize, tileSize)
      selectSquare.setFill(Color.CHARTREUSE)
      selectSquare.setOpacity(0.2)
      getChildren.add(selectSquare)

      if (moveSquares != null)
        moveSquares.foreach(ms => {
          getChildren.remove(ms)
        })

      moveOptions.foreach(mo => {
        val mos = new Rectangle(selected._1 * tileSize, selected._2 * tileSize, tileSize, tileSize)
        mos.setFill(Color.CHARTREUSE)
        mos.setOpacity(0.2)
        getChildren.add(mos)
      })

    }

  }

  def drawTerrainTile(x: Int, y: Int, gc: GraphicsContext): Unit = {
    val t = query.getTerrainTile(x, y)
    if (!t.eq(VoidTile)) {
      gc.drawImage(images(t.char), 0, 0, tileSize, tileSize)
    }
  }

  def drawUnitTile(x: Int, y: Int, gc: GraphicsContext): Unit = {

    val t = query.getUnitTile(x, y)

    if (!t.eq(VoidTile)) {
      if (!query.isUnitOnline(x, y))
        gc.setGlobalAlpha(0.3)
      gc.drawImage(images(t.char), 0, 0, tileSize, tileSize)

      gc.setGlobalAlpha(1)
      if (t.isBlue)
        gc.setFill(Color.BLUE)
      else
        gc.setFill(Color.RED)
      gc.fillRect(0, tileSize - tileSize / 15, tileSize, tileSize / 15)

    }
  }

  def drawComTile(c: (mutable.Set[Direction], mutable.Set[Direction]), gc: GraphicsContext): Unit = {

    val red = c._1
    val blue = c._2
    gc.setLineWidth(4)
    gc.setGlobalAlpha(0.05)

    def doDraw(dirs: mutable.Set[Direction]) = {
      dirs.foreach(_ match {
        case N | S => {
          gc.moveTo(tileSize / 2, 0)
          gc.lineTo(tileSize / 2, tileSize)
        }
        case NE | SW => {
          gc.moveTo(0, tileSize)
          gc.lineTo(tileSize, 0)
        }
        case E | W => {
          gc.moveTo(0, tileSize / 2)
          gc.lineTo(tileSize, tileSize / 2)
        }
        case SE | NW => {
          gc.moveTo(0, 0)
          gc.lineTo(tileSize, tileSize)
        }
      })
      gc.stroke
    }

    gc.setStroke(Color.RED)
    doDraw(red)

    gc.setStroke(Color.BLUE)
    doDraw(blue)

  }

  def drawTileLayers(x: Int, y: Int): Unit = {

    val tileSquare = new Rectangle(x * tileSize, y * tileSize, tileSize, tileSize)
    if ((x + y) % 2 == 0) {
      tileSquare.setFill(Color.WHITE)
    } else {
      tileSquare.setFill(Color.WHITESMOKE)
    }
    getChildren add tileSquare

    val terrainCanvas = new Canvas(tileSize, tileSize)
    val terrainGc = terrainCanvas.getGraphicsContext2D
    if (Random.nextBoolean())
      terrainCanvas.setScaleX(-1)
    terrainCanvas.setLayoutX(x * tileSize)
    terrainCanvas.setLayoutY(y * tileSize)
    getChildren add terrainCanvas

    drawTerrainTile(x, y, terrainGc)

    val unitCanvas = new Canvas(tileSize, tileSize)
    val unitGc = unitCanvas.getGraphicsContext2D
    if (Random.nextBoolean())
      unitCanvas.setScaleX(-1)
    unitCanvas.setLayoutX(x * tileSize)
    unitCanvas.setLayoutY(y * tileSize)
    getChildren add unitCanvas

    drawUnitTile(x, y, unitGc)

    val comCanvas = new Canvas(tileSize, tileSize)
    val comGc = comCanvas.getGraphicsContext2D
    comCanvas.setLayoutX(x * tileSize)
    comCanvas.setLayoutY(y * tileSize)
    getChildren add comCanvas

    drawComTile(query.getComTile(x, y), comGc)

    comCanvas.setOnMouseClicked(new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent) = onTileClicked(event)
    })

    /*def setPerspective(p: Pane): Unit = {
      val e = new PerspectiveTransform()
      e.setUlx(tileSize * 2)
      e.setUly(0)
      e.setUrx(width - tileSize * 2)
      e.setUry(0)
      e.setLlx(0)
      e.setLly(height)
      e.setLrx(width)
      e.setLry(height)
      p.setEffect(e)
    }*/

  }

  def scanBoardCoordinates(func: (Int, Int) => Unit) {
    (0 until Rules.terrainWidth).foreach(x => {
      (0 until Rules.terrainHeight).foreach(y => {
        func(x, y)
      })
    })
  }

  scanBoardCoordinates(drawTileLayers)

  val listener = new InvalidationListener {
    override def invalidated(o: beans.Observable) {
      scanBoardCoordinates(drawTileLayers)
    }
  }
  widthProperty addListener (listener);
  heightProperty addListener (listener);


}
