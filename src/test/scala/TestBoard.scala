import com.lms.gow.model.{Rules, VoidTile, Tile, Game}
import Tile._
import org.scalatest._

class TestBoard extends FlatSpec with Matchers {

  "Initial Units" should "be well placed" in {
    val board = new Game()
    val topUnit = Rules.startingUnits.take((Rules.terrainWidth * Rules.terrainHeight) / 2)
    val bottomUnit = Rules.startingUnits.reverse.take((Rules.terrainWidth * Rules.terrainHeight) / 2)

    println(topUnit.map(_.char).filterNot(_.equals(VoidTile.char)))
    println(bottomUnit.map(_.char).filterNot(_.equals(VoidTile.char)))

    assert(topUnit.length == bottomUnit.length)

  }

}