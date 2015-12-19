
package com.lms.gow

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.effect.PerspectiveTransform
import javafx.scene.image.Image
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage

import com.lms.gow.model._

import scala.util.Random

class App extends Application {

  val model = new Board
  val tileSize = 50
  val width = tileSize * Rules.terrainWidth
  val height = tileSize * Rules.terrainHeight

  def createBoardPane = {

    val boardPane = new Pane

    def createTileSquare(x: Int, y: Int) = {
      val tileSquare = new Rectangle(x * tileSize, y * tileSize, tileSize, tileSize);
      if ((x + y) % 2 == 0) {
        tileSquare.setFill(Color.WHITE)
      } else {
        tileSquare.setFill(Color.WHITESMOKE)
      }
      boardPane.getChildren().add(tileSquare)
    }

    def createTile(x: Int, y: Int) = {

      val canvas = new Canvas(tileSize, tileSize)
      val gc = canvas.getGraphicsContext2D()
      if (Random.nextBoolean())
        canvas.setScaleX(-1)
      canvas.setLayoutX(x * tileSize)
      canvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(canvas)

      def drawTile(t: Tile): Unit = {
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

          // Draw lines of communication
          if (t.isCom) {
            val canvas2 = new Canvas(width, height)
            val gc2 = canvas2.getGraphicsContext2D
            if (t.isPlayer1)
              gc2.setStroke(Color.BLUE)
            else
              gc2.setStroke(Color.RED)

            val tileCenter = (x * tileSize + tileSize / 2, y * tileSize + tileSize / 2)

            gc2.strokeLine(tileCenter._1, 0, tileCenter._1, height)
            gc2.strokeLine(0, tileCenter._2, width, tileCenter._2)

            boardPane.getChildren().add(canvas2)
          }

        }
      }

      drawTile(model.getTile(x, y)._1)
      drawTile(model.getTile(x, y)._2)

    }

    (0 until Rules.terrainWidth).foreach(x => {
      (0 until Rules.terrainHeight).foreach(y => {
        createTileSquare(x, y)

      })
    })

    (0 until Rules.terrainWidth).foreach(x => {
      (0 until Rules.terrainHeight).foreach(y => {
        createTile(x, y)
      })
    })

    boardPane
  }

  def start(stage: Stage) {

    model.move(14, 10, 13, 10)

    val board = createBoardPane
    val e = new PerspectiveTransform()
    e.setUlx(tileSize * 2)
    e.setUly(0)
    e.setUrx(width - tileSize * 2)
    e.setUry(0)
    e.setLlx(0)
    e.setLly(height)
    e.setLrx(width)
    e.setLry(height)
    board.setEffect(e)

    val scene = new Scene(board, Color.WHITESMOKE)

    stage setTitle ("Game of War")
    stage setScene (scene)
    stage show
  }

}

object App {

  def main(args: Array[String]) {
    Application.launch(classOf[App], args: _*)
  }

}
