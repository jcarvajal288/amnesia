import nodes.Node1
import util.GameContext

object Main {

  def main(args: Array[String]): Unit = {
    val gameContext = new GameContext
    Node1.begin(gameContext)
  }
}
