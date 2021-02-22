import nodes.Node1
import util.GameContext

object Main {

  def main(args: Array[String]): Unit = {
    new Node1(new GameContext).begin()
  }
}
