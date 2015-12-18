
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

class App extends Application {

  val boardModel = new Board
  val tileSize = 50
  val width = tileSize * boardModel.terrainWidth
  val height = tileSize * boardModel.terrainHeight

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
      canvas.setLayoutX(x * tileSize)
      canvas.setLayoutY(y * tileSize)
      boardPane.getChildren().add(canvas)

      def drawTile(t: Tile): Unit = {
        if (!t.eq(VoidTile)) {
          val tileImage = new Image(t.char.toString + ".png")
          gc.drawImage(tileImage, 0, 0, tileSize, tileSize)

          if (Seq(Arsenal, Relay, SwiftRelay).contains(t)) {
            // Draw lines of communication
          }

        }
      }

      drawTile(boardModel.getTile(x, y)._1)
      drawTile(boardModel.getTile(x, y)._2)

    }

    (0 until boardModel.terrainWidth).foreach(x => {
      (0 until boardModel.terrainHeight).foreach(y => {
        createTileSquare(x, y)
        createTile(x, y)
      })
    })

    boardPane
  }

  def start(stage: Stage) {

    val board = createBoardPane
    val e = new PerspectiveTransform()
    e.setUlx(tileSize*2)
    e.setUly(0)
    e.setUrx(width - tileSize*2)
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
