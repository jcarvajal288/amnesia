package nodes

import text.Nightmare1Text._
import util.GameContext
import util.UserInterface._

class Nightmare1(gameContext: GameContext) {

  def begin(): Unit = {
    startDream(text1)
  }

  def startDream(text: String): Unit = {
    promptChoice(text, List(
      "Go to mirror" -> "GO_TO_MIRROR",
      "Run away" -> "RUN_AWAY",
      "Go back to sleep" -> "SLEEP",
      "Talk to voice" -> "TALK"
    )) match {
      case "RUN_AWAY" =>
        loopMirrorResponse("run away")
      case "SLEEP" =>
        loopMirrorResponse("ignore the voice")
      case "TALK" =>
        loopMirrorResponse("talk")
      case "GO_TO_MIRROR" =>
        goToMirror(text2)
    }
  }

  def loopMirrorResponse(response: String): Unit = {
    printAndWait(
      text1a.replace("(response)", response)
    )
    startDream("What now?")
  }

  def goToMirror(text: String): Unit = {
    promptChoice(text, List(
      "Enter mirror" -> "ENTER_MIRROR",
      "Break mirror" -> "BREAK_MIRROR",
      "Talk to voice" -> "TALK",
      "Run away" -> "RUN_AWAY"
    )) match {
      case "RUN_AWAY" =>
        loopMirrorResponse("run away")
      case "BREAK_MIRROR" =>
        loopMirrorResponse("break the mirror")
      case "TALK" =>
        loopMirrorResponse("talk")
      case "ENTER_MIRROR" =>
        enterMirror()
    }
  }

  def enterMirror(): Unit = {
    val response = promptFreeformResponse(text3, "What do you write? ")
    if (containsObscenities(response)) {
      obsceneGraffito()
    } else if (response.equals("X")) {
      printAndWait(text6)
      exitNightmare()
    } else {
      departmentStore(text7)
    }
  }

  def obsceneGraffito(): Unit = {
    val name = promptFreeformResponse(text4, "Name a person: ")
    val filledText5 = text5.replace("(name)", name)
    val filledText5a = text5a.replace("(name)", name)
    printAndWait(filledText5)
    printAndWait(filledText5a)
    exitNightmare()
  }

  def exitNightmare(): Unit = ???

  def departmentStore(text: String): Unit = {
    promptChoice(text, List(
      "Take escalator" -> "TAKE_ESCALATOR",
      "Follow companion" -> "FOLLOW_COMPANION",
      "Look around first floor" -> "LOOK_AROUND"
    )) match {
      case "FOLLOW_COMPANION" =>
        escalatorLoop("follow your companion")
      case "LOOK_AROUND" =>
        escalatorLoop("look around more of the first floor")
      case "TAKE_ESCALATOR" =>
        List(text8, text9, text10, text11)
        .foreach(takeEscalator)
        fifthFloor()
        takeEscalator(text13)
        takeEscalator(text14)
        eighthFloor()
        ninthFloor()
        tenthFloor()
    }
  }

  def escalatorLoop(response: String): Unit = {
    printAndWait(text7a.replace("(response)", response))
    startDream("What now?")
  }

  def takeEscalator(text: String): Unit = {
    promptChoice(text, List(
      "Take escalator" -> "TAKE_ESCALATOR"
    ))
  }

  def fifthFloor(): Unit = {
    promptChoice(text12, List(
      "'Yes'" -> "YES",
      "'No'" -> "NO",
      "Take escalator" -> "TAKE_ESCALATOR"
    )) match {
      case "YES" =>
        printAndWait(text12a)
        takeEscalator(text13)
      case "NO" =>
        printAndWait(text12b)
        takeEscalator(text13)
      case "TAKE_ESCALATOR" =>
        takeEscalator(text13)
    }
  }

  def eighthFloor(): Unit = {
    promptChoice(text15, List(
      "Sit in chair" -> "SIT",
      "Take escalator" -> "TAKE_ESCALATOR"
    )) match {
      case "SIT" =>
        printAndWait(text15a)
        printAndWait(text15b)
    }
  }

  def ninthFloor(): Unit = {
    val response = promptFreeformResponse(
      text16, "Fill in the blank: "
    )
    if (response.toUpperCase.equals("A RIDDLE")) {
      promptChoice(text16b, List(
        "Look for switch" -> "SWITCH",
        "Take escalator" -> "TAKE_ESCALATOR"
      )) match {
        case "SWITCH" =>
          printAndWait(text16c)
          exitNightmare()
      }
    }
  }

  def tenthFloor(): Unit = {
    promptChoice(text17, List(
      "Take escalator" -> "TAKE_ESCALATOR"
    ))
    promptChoice(text18, List(
      "Refuse" -> "REFUSE",
      "Take escalator" -> "TAKE_ESCALATOR",
      "Follow security officer" -> "FOLLOW_OFFICER"
    )) match {
      case "REFUSE" || "TAKE_ESCALATOR" =>
        List(text19, text20, text20a, text20b, text20c, text20d)
        .foreach(twelfthFloor)
      case "FOLLOW_OFFICER" =>
        printAndWait(text18a)
        exitNightmare()
    }
  }

  def twelfthFloor(text: String): Unit = {
    promptChoice(text, List(
      "Open box" -> "BOX"
    ))
  }
}
