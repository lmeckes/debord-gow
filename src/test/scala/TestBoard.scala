import com.lms.gow.model.{VoidTile, Board}
import org.scalatest._

class TestBoard extends FlatSpec with Matchers {

  "Board" should "print" in {
    val board = new Board()
    board.dump
  }

  "Initial Units" should "be well placed" in {
    val board = new Board()
    val topUnit = board.Config.startingUnits.take((board.Config.terrainWidth * board.Config.terrainHeight) / 2)
    val bottomUnit = board.Config.startingUnits.reverse.take((board.Config.terrainWidth * board.Config.terrainHeight) / 2)

    println(topUnit.map(_.char).filterNot(_.equals(VoidTile.char)))
    println(bottomUnit.map(_.char).filterNot(_.equals(VoidTile.char)))

    assert(topUnit.length == bottomUnit.length)

  }

}