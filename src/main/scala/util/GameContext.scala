package util

class GameContext {

  private val valueStore: Map[String, String] = Map()

  def storeValue(key: String, value: String): Unit = {
    valueStore + (key -> value)
  }

  def readValue(key: String): Option[String] = {
    valueStore get key
  }
}
