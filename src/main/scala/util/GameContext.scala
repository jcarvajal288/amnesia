package util

import scala.collection.mutable

class GameContext {

  private val valueStore: mutable.Map[String, String] = mutable.Map()

  def storeValue(key: String, value: String): Unit = {
    valueStore.addOne(key -> value)
  }

  def readValue(key: String): Option[String] = {
    valueStore get key
  }

  def valueSetAs(key: String, value: String): Boolean = {
    (valueStore contains key) && readValue(key).get == value
  }
}
