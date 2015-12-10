import main.scala.com.lms.gow.Board
import org.scalatest._

class TestBoard extends FlatSpec with Matchers {

  "Board" should "print" in {
    val board = new Board()
    board.printCurrentBoard
  }

}