package util

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object UserInterface {

  def printAndWait(text: String): String = {
    println(text)
    println("")
    print("Press enter to continue.")
    readLine()
  }

  def readInput(): String = {
    println("")
    print(">")
    readLine()
  }

  @tailrec
  def promptChoice(text: String, choices: List[(String, String)]): String = {
    println(text)
    println("")
    choices
      .zipWithIndex
      .foreach{case (choice: (String, String), index: Int) =>
        println(s"${index + 1}) ${choice._1}")
      }
    println("")
    print("Enter number of your choice: ")
    val selection = readLine()
    try {
      choices.apply(selection.toInt - 1)._2
    } catch {
      case _: NumberFormatException =>
        println("Please input a number.")
        promptChoice(text, choices)
    }
  }

  def promptFreeformResponse(text: String, prompt: String): String = {
    println(text)
    println("")
    print(prompt)
    readLine()
  }

  def promptForResponses(text: String, numResponses: Int): List[String] = {
    for (i <- (1 to numResponses).toList) yield {
      print(s"Enter response #$i: ")
      readLine()
    }
  }

  def containsObscenities(text: String): Boolean = {
    List(
      "FUCK",
      "SHIT",
      "DAMN",
      "CUNT",
      "JIZZ",
      "COCK",
      "DICK",
      "TURD"
    ).exists(text.toUpperCase.contains(_))
  }
}
