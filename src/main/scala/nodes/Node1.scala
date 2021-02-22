package nodes

import util.GameContext
import util.Parser._

object Node1 {

  def begin(gameContext: GameContext): Unit = {
    printAndWait(Node1Text.text1)
    printAndWait(Node1Text.text2)
    printAndMakeChoice(
      Node1Text.text3, List(
      "Get Up" -> "GET_UP",
      "Go back to sleep" -> "SLEEP"
    )) match {
      case "SLEEP" => sleep(gameContext)
      case "GET_UP" => outOfBed(gameContext)
    }
  }

  def sleep(gameContext: GameContext): Unit = {
    throw new RuntimeException("Off to NODE 2")
  }

  def outOfBed(gameContext: GameContext): Unit = {
    printAndWait(Node1Text.text4)
    val hairColor = printAndMakeChoice(
      Node1Text.text4a, List(
      "Light" -> "dark",
      "Dark" -> "light"
    ))
    val hairLength = printAndMakeChoice(
      Node1Text.text4b, List(
      "Long" -> "short",
      "Short" -> "long"
    ))
    val beardType = printAndMakeChoice(
      Node1Text.text5, List(
      "Beard" -> "a mustache",
      "Mustache" -> "neither a beard nor a mustache",
      "Neither" -> "a beard"
    ))
    print()
    val eyeColor = printAndMakeChoice(
      Node1Text.text6, List(
      "Blue" -> "brown",
      "Brown" -> "green",
      "Green" -> "blue"
    ))
    val filledInText7 = Node1Text.text7
      .replace("(hairColor)", hairColor)
      .replace("(hairLength)", hairLength)
      .replace("(beardType)", beardType)
      .replace("(eyeColor)", eyeColor)
    printAndWait(filledInText7)
  }
}
