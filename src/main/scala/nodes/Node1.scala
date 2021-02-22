package nodes

import nodes.Node1Text._
import util.GameContext
import util.UserInterface._

import scala.annotation.tailrec

class Node1(gameContext: GameContext) {

  def begin(): Unit = {
    printAndWait(text1)
    printAndWait(text2)
    promptChoice(
      text3, List(
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
    printAndWait(text4)
    val hairColor = promptChoice(
      text4a, List(
      "Light" -> "dark",
      "Dark" -> "light"
    ))
    val hairLength = promptChoice(
      text4b, List(
      "Long" -> "short",
      "Short" -> "long"
    ))
    val beardType = promptChoice(
      text5, List(
      "Beard" -> "a mustache",
      "Mustache" -> "neither a beard nor a mustache",
      "Neither" -> "a beard"
    ))
    print()
    val eyeColor = promptChoice(
      text6, List(
      "Blue" -> "brown",
      "Brown" -> "green",
      "Green" -> "blue"
    ))
    val filledInText7 = text7
      .replace("(hairColor)", hairColor)
      .replace("(hairLength)", hairLength)
      .replace("(beardType)", beardType)
      .replace("(eyeColor)", eyeColor)
    printAndWait(filledInText7)
    knockOnDoor()
  }

  def knockOnDoor(): Unit = {
    promptChoice(
      text8, List(
        "Get dressed" -> "DRESSED",
        "Look around" -> "LOOK"
      )
    ) match {
      case "DRESSED" =>
        printAndWait(text9)
        lookForClothes(text9a)
      case "LOOK" =>
        lookForClothes(text9a)
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
        printAndWait(text9b)
        lookForClothes("Look where?")
      case "DRESSER" =>
        printAndWait(text9c)
        printAndWait(text10)
        lookForClothes("Look where?")
      case "CLOSET" =>
        lookInCloset()
      case "BATHROOM" =>
        printAndWait(text12a)
        lookInBathroom()
    }
  }

  def lookInCloset(): Unit = {
    promptChoice(
      text11, List(
        "Wear blanket" -> "BLANKET",
        "Look elsewhere" -> "LOOK_ELSEWHERE"
      )
    ) match {
      case "BLANKET" =>
        gameContext.storeValue("wearing", "blanket")
        printAndWait(text12)
        takeStock(text18)
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
      text13, choices
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
        takeStock(text18)
    }
    takeStock(text18)
  }

  def bathroomDally(action: String): Unit = {
    printAndWait(
      text17b.replace(
        "(bathroomAction)", action
      )
    )
  }

  def takeTowel(): Unit = {
    gameContext.storeValue("wearing", "towel")
    promptChoice(
      text14, List(
        "Yes" -> "YES",
        "No" -> "NO"
      )
    ) match {
      case "YES" => takeStock(text18)
      case "NO" => lookInBathroom()
    }
  }

  def lookInToilet(): Unit = {
    val response: String = promptFreeformResponse(text15)
    if (response.toUpperCase.contains("SHIT")) {
      printAndWait(text16)
      goToNode2()
    } else {
      promptChoice(
        text17, List(
          "Keep looking" -> "STAY",
          "Return to room" -> "EXIT"
        )
      ) match {
        case "STAY" => lookInBathroom()
        case "EXIT" => takeStock(text18)
      }
    }
  }

  def takeStock(text: String): Unit = {
    promptChoice(
      text, List(
        "Open drapes" -> "DRAPES",
        "Examine room" -> "EXAMINE_ROOM"
      )
    ) match {
      case "DRAPES" =>
        printAndWait(text19)
        printAndWait(text20)
        takeStock("What now?")
      case "EXAMINE_ROOM" =>
        printAndWait(text22)
        printAndWait(text23)
        phoneCall(text24)
    }
  }

  def phoneCall(text: String): Unit = {
    promptChoice(
      text, List(
        "Answer phone" -> "PHONE",
        "Not now" -> "LATER"
      )
    ) match {
      case "PHONE" => answerPhone()
      case "LATER" => phoneCall(text24a)
    }
  }

  def answerPhone(): Unit = {
    promptChoice(text25, List(
      "'Yes'" -> "YES",
      "'No'" -> "NO"
    ))
    promptChoice(text26, List(
      "'Yes'" -> "YES",
      "'No'" -> "NO"
    )) match {
      case "NO" =>
        printAndWait(text27)
        askAboutAmericanExpress()
      case "YES" => askAboutAmericanExpress()
    }
  }

  def askAboutAmericanExpress(): Unit = {
    promptChoice(text27a, List(
      "'Yes'" -> "YES",
      "'No'" -> "NO"
    )) match {
      case "YES" => sendBellboy()
      case "NO" =>
        printAndWait(text27b)
        sendBellboy()
    }
  }

  def sendBellboy(): Unit = ???

  def goToNode2(): Unit = {
    printAndWait("Off to Node 2")
  }

}
