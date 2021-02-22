package nodes

import util.GameContext
import util.Parser._

object Node1 {

  def begin(gameContext: GameContext): Unit = {
    print(Node1Text.text1)
    waitForUser()
    print(Node1Text.text2)
    waitForUser()
    print(Node1Text.text3)
    makeChoice(List(
      "Get Up" -> "GET_UP",
      "Go back to sleep" -> "SLEEP"
    )) match {
      case "SLEEP" => sleep(gameContext)
      case "GET_UP" => getUp(gameContext)
    }
  }

  def sleep(gameContext: GameContext): Unit = {
    throw new RuntimeException("Off to NODE 2")
  }

  def getUp(gameContext: GameContext): Unit = {
    print(Node1Text.text4)
    print(Node1Text.text4a)
    val hairColor = makeChoice(List(
      "Light" -> "dark",
      "Dark" -> "light"
    ))
    print(Node1Text.text4b)
    val hairLength = makeChoice(List(
      "Long" -> "short",
      "Short" -> "long"
    ))
    print(Node1Text.text5)
    val beardType = makeChoice(List(
      "Beard" -> "a mustache",
      "Mustache" -> "neither a beard nor a mustache",
      "Neither" -> "a beard"
    ))
    print(Node1Text.text6)
    val eyeColor = makeChoice(List(
      "Blue" -> "brown",
      "Brown" -> "green",
      "Green" -> "blue"
    ))
    val filledInText7 = Node1Text.text7
      .replace("(hairColor)", hairColor)
      .replace("(hairLength)", hairLength)
      .replace("(beardType)", beardType)
      .replace("(eyeColor)", eyeColor)
    print(filledInText7)
  }
}
