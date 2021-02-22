package util

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Parser {

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
  def printAndMakeChoice(text: String, choices: List[(String, String)]): String = {
    println(text)
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
        println("Please input a number.")
        printAndMakeChoice(text, choices)
      }
    }
  }
}
