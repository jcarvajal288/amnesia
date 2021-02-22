package util

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Parser {

  def print(text: String): Unit = {
    System.out.println(text)
  }

  def waitForUser(): String = {
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
  def makeChoice(choices: List[(String, String)]): String = {
    println("")
    choices
      .zipWithIndex
      .foreach{case (choice: (String, String), index: Int) =>
        println(s"${index}) ${choice._1}")
      }
    println("")
    print("Enter number of your choice: ")
    val selection = readLine()
    try {
      choices.apply(selection.toInt)._2
    } catch {
      case _: NumberFormatException => {
        makeChoice(choices)
      }
    }
  }
}
