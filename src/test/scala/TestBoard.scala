import com.lms.gow.model.{VoidTile, Board}
import org.scalatest._

class TestBoard extends FlatSpec with Matchers {

  "Board" should "print" in {
    val board = new Board()
    board.printCurrentBoard
  }

  "Initial Units" should "be well placed" in {
    val board = new Board()
    val topUnit = board.startingUnits.take((board.terrainWidth * board.terrainHeight) / 2)
    val bottomUnit = board.startingUnits.reverse.take((board.terrainWidth * board.terrainHeight) / 2)

    println(topUnit.map(_.char).filterNot(_.equals(VoidTile.char)))
    println(bottomUnit.map(_.char).filterNot(_.equals(VoidTile.char)))

    assert(topUnit.length == bottomUnit.length)

  }

}