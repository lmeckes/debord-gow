
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

      val canvas = new Canvas(tileSize, tileSize)
      val gc = canvas.getGraphicsContext2D
      if (Random.nextBoolean())
        canvas.setScaleX(-1)
      canvas.setLayoutX(x * tileSize)
      canvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(canvas)

      drawTile(model.getTerrainTile(x, y), gc)
      drawTile(model.getUnitTile(x, y), gc)

      val canvas2 = new Canvas(tileSize, tileSize)
      val gc2 = canvas2.getGraphicsContext2D
      canvas2.setLayoutX(x * tileSize)
      canvas2.setLayoutY(y * tileSize)
      boardPane.getChildren().add(canvas2)

      drawCom(model.getComTile(x, y), gc2)

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

    def drawTile(t: Tile, gc: GraphicsContext): Unit = {
      if (!t.eq(VoidTile)) {

        val tileImage = new Image("tiles/" + t.char.toString + ".png")
        gc.drawImage(tileImage, 0, 0, tileSize, tileSize)

        // Draw color bar
        if (t.isUnit) {
          if (t.isPlayer1)
            gc.setFill(Color.BLUE)
          else
            gc.setFill(Color.RED)
          gc.fillRect(0, tileSize - tileSize / 15, tileSize, tileSize / 15)
        }

      }
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
