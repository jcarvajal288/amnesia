package nodes

import util.GameContext
import util.UserInterface._

import scala.annotation.tailrec

class Node1(gameContext: GameContext) {

  def begin(): Unit = {
    printAndWait(Node1Text.text1)
    printAndWait(Node1Text.text2)
    promptChoice(
      Node1Text.text3, List(
      "Get Up" -> "GET_UP",
      "Go back to sleep" -> "SLEEP"
    )) match {
      case "SLEEP" => sleep()
      case "GET_UP" => outOfBed()
    }
  }

  def sleep(): Unit = {
    goToNode2()
  }

  def outOfBed(): Unit = {
    printAndWait(Node1Text.text4)
    val hairColor = promptChoice(
      Node1Text.text4a, List(
      "Light" -> "dark",
      "Dark" -> "light"
    ))
    val hairLength = promptChoice(
      Node1Text.text4b, List(
      "Long" -> "short",
      "Short" -> "long"
    ))
    val beardType = promptChoice(
      Node1Text.text5, List(
      "Beard" -> "a mustache",
      "Mustache" -> "neither a beard nor a mustache",
      "Neither" -> "a beard"
    ))
    print()
    val eyeColor = promptChoice(
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
    knockOnDoor()
  }

  def knockOnDoor(): Unit = {
    promptChoice(
      Node1Text.text8, List(
        "Get dressed" -> "DRESSED",
        "Look around" -> "LOOK"
      )
    ) match {
      case "DRESSED" =>
        printAndWait(Node1Text.text9)
        lookForClothes(Node1Text.text9a)
      case "LOOK" =>
        lookForClothes(Node1Text.text9a)
    }
  }

  @tailrec
  final def lookForClothes(text: String): Unit = {
    promptChoice(
      text, List(
        "Look in closet" -> "CLOSET",
        "Look in bathroom" -> "BATHROOM",
        "Look under bed" -> "BED",
        "Look in dresser" -> "DRESSER",
      )
    ) match {
      case "BED" =>
        printAndWait(Node1Text.text9b)
        lookForClothes("Look where?")
      case "DRESSER" =>
        printAndWait(Node1Text.text9c)
        printAndWait(Node1Text.text10)
        lookForClothes("Look where?")
      case "CLOSET" =>
        lookInCloset()
      case "BATHROOM" =>
        printAndWait(Node1Text.text12a)
        lookInBathroom()
    }
  }

  def lookInCloset(): Unit = {
    promptChoice(
      Node1Text.text11, List(
        "Wear blanket" -> "BLANKET",
        "Look elsewhere" -> "LOOK_ELSEWHERE"
      )
    ) match {
      case "BLANKET" =>
        gameContext.storeValue("wearing", "blanket")
        printAndWait(Node1Text.text12)
        takeStock()
      case "LOOK_ELSEWHERE" =>
        lookForClothes("Look where?")
    }
  }

  def lookInBathroom(): Unit = {
    var choices = List(
      "Look in the toilet" -> "LOOK_IN_TOILET",
      "Take bath" -> "BATH",
      "Wash face" -> "WASH_FACE",
      "Use the toilet" -> "USE_TOILET",
      "Leave the bathroom" -> "LEAVE"
    )
    if(!gameContext.valueSetAs("wearing", "towel")) {
      choices = choices.+:("Wear Towel" -> "TOWEL")
    }
    promptChoice(
      Node1Text.text13, choices
    ) match {
      case "TOWEL" =>
        takeTowel()
      case "LOOK_IN_TOILET" =>
        lookInToilet()
      case "BATH" =>
        bathroomDally("take a bath")
        lookInBathroom()
      case "WASH_FACE" =>
        bathroomDally("wash your face")
        lookInBathroom()
      case "USE_TOILET" =>
        bathroomDally("use the toilet")
        lookInBathroom()
      case "LEAVE" =>
        takeStock()
    }
    takeStock()
  }

  def bathroomDally(action: String): Unit = {
    printAndWait(
      Node1Text.text17b.replace(
        "(bathroomAction)", action
      )
    )
  }

  def takeTowel(): Unit = {
    gameContext.storeValue("wearing", "towel")
    promptChoice(
      Node1Text.text14, List(
        "Yes" -> "YES",
        "No" -> "NO"
      )
    ) match {
      case "YES" => takeStock()
      case "NO" => lookInBathroom()
    }
  }

  def lookInToilet(): Unit = {
    val response: String = promptFreeformResponse(Node1Text.text15)
    if (response.toUpperCase.contains("SHIT")) {
      printAndWait(Node1Text.text16)
      goToNode2()
    } else {
      promptChoice(
        Node1Text.text17, List(
          "Keep looking" -> "STAY",
          "Return to room" -> "EXIT"
        )
      ) match {
        case "STAY" => lookInBathroom()
        case "EXIT" => takeStock()
      }
    }
  }

  def takeStock(): Unit = ???

  def goToNode2(): Unit = {
    printAndWait("Off to Node 2")
  }

}
