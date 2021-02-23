package util

import scala.collection.mutable

class GameContext {

  private val valueStore: mutable.Map[Constants.Value, String] = mutable.Map()

  def storeValue(key: Constants.Value, value: String): Unit = {
    valueStore.addOne(key -> value)
  }

  def readValue(key: Constants.Value): Option[String] = {
    valueStore get key
  }

  def valueSetAs(key: Constants.Value, value: String): Boolean = {
    (valueStore contains key) && readValue(key).get == value
  }

  def setFlag(key: Constants.Value): Unit = {
    valueStore.addOne(key -> "x")
  }

  def testFlag(key: Constants.Value): Boolean = {
    valueStore contains key
  }
}
