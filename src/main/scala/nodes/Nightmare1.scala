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
      rideSubway()
    }
  }
}
