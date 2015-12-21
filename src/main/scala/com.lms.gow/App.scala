
package com.lms.gow

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.effect.PerspectiveTransform
import javafx.scene.image.Image
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage

import com.lms.gow.model._

import scala.collection._
import scala.util.Random

class App extends Application {

  val model = new Board
  val tileSize = 50
  val width = tileSize * Rules.terrainWidth
  val height = tileSize * Rules.terrainHeight

  def start(stage: Stage) {

    model.move(14, 10, 13, 10)
    model.move(22, 14, 23, 14)

    val board = createBoardPane
    setPerspective(board)
    val scene = new Scene(board, Color.WHITESMOKE)

    stage setTitle ("Game of War")
    stage setScene (scene)
    stage show
  }

  def createBoardPane = {

    val boardPane = new Pane

    def scanBoardCoordinates(func: (Int, Int) => Unit) {
      (0 until Rules.terrainWidth).foreach(x => {
        (0 until Rules.terrainHeight).foreach(y => {
          func(x, y)
        })
      })
    }

    def drawTiles(x: Int, y: Int): Unit = {

      val tileSquare = new Rectangle(x * tileSize, y * tileSize, tileSize, tileSize);
      if ((x + y) % 2 == 0) {
        tileSquare.setFill(Color.WHITE)
      } else {
        tileSquare.setFill(Color.WHITESMOKE)
      }
      boardPane.getChildren().add(tileSquare)

      val terrainCanvas = new Canvas(tileSize, tileSize)
      val terrainGc = terrainCanvas.getGraphicsContext2D
      if (Random.nextBoolean())
        terrainCanvas.setScaleX(-1)
      terrainCanvas.setLayoutX(x * tileSize)
      terrainCanvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(terrainCanvas)

      drawTerrainTile(x, y, terrainGc)

      val unitCanvas = new Canvas(tileSize, tileSize)
      val unitGc = unitCanvas.getGraphicsContext2D
      if (Random.nextBoolean())
        unitCanvas.setScaleX(-1)
      unitCanvas.setLayoutX(x * tileSize)
      unitCanvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(unitCanvas)

      drawUnitTile(x, y, unitGc)

      val comCanvas = new Canvas(tileSize, tileSize)
      val comGc = comCanvas.getGraphicsContext2D
      comCanvas.setLayoutX(x * tileSize)
      comCanvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(comCanvas)

      drawCom(model.getComTile(x, y), comGc)

    }

    def drawTerrainTile(x: Int, y: Int, gc: GraphicsContext): Unit = {
      val t = model.getTerrainTile(x, y)
      if (!t.eq(VoidTile)) {
        val tileImage = new Image("tiles/" + t.char.toString + ".png")
        gc.drawImage(tileImage, 0, 0, tileSize, tileSize)
      }
    }

    def drawUnitTile(x: Int, y: Int, gc: GraphicsContext): Unit = {

      val t = model.getUnitTile(x, y)

      if (!t.eq(VoidTile)) {

        if (!model.hasCom(x, y))
          gc.setGlobalAlpha(0.3)

        val tileImage = new Image("tiles/" + t.char.toString + ".png")
        gc.drawImage(tileImage, 0, 0, tileSize, tileSize)

        if (t.isBlue)
          gc.setFill(Color.BLUE)
        else
          gc.setFill(Color.RED)
        gc.fillRect(0, tileSize - tileSize / 15, tileSize, tileSize / 15)

      }
    }

    def drawCom(c: (mutable.Set[Direction], mutable.Set[Direction]), gc: GraphicsContext): Unit = {

      val red = c._1
      val blue = c._2
      gc.setLineWidth(4)
      gc.setGlobalAlpha(0.1)

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
      }

      gc.setStroke(Color.RED)
      doDraw(red)
      gc.stroke
      gc.setStroke(Color.BLUE)
      doDraw(blue)
      gc.stroke

    }

    // Draw Terrain
    scanBoardCoordinates(drawTiles)

    boardPane
  }

  def setPerspective(p: Pane): Unit = {
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
  }

}

object App {

  def main(args: Array[String]) {
    Application.launch(classOf[App], args: _*)
  }

}
