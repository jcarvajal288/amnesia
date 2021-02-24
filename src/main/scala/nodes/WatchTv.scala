package nodes

import nodes.WatchTvText._
import util.GameContext
import util.UserInterface._
import util.Constants._

import scala.util.Random

class WatchTv(gameContext: GameContext) {

  private val country = Random.shuffle(List(
    "El Salvador", "Guatemala", "Honduras", "the Philippines", "Chile"
  )).head

  private val numDead = Random.shuffle(List(
    "Two", "Three", "Four", "Five"
  )).head

  private val neighborhood = Random.shuffle(List(
    "Brooklyn", "Queens", "the Bronx"
  )).head

  def tvSelect(text: String, nextChannel: () => Unit): Unit = {
    promptChoice(text, List(
      "ON" -> "ON",
      "OFF" -> "OFF",
      "FORWARD" -> "FORWARD"
    )) match {
      case "ON" =>
        tvSelect(text36a, nextChannel)
      case "OFF" => // return
      case "FORWARD" =>
        nextChannel()
    }
  }

  def begin(): Unit = {
    tvSelect(text35, watchChannel2)
  }

  def watchChannel2(): Unit = {
    val drink = Random.shuffle(List(
      "Kool-aid", "Coke", "Pepsi", "Crystal-Lite"
    )).head
    val consonants: List[Char] =
      Random.shuffle("BCDFGHJKLMNPRSTVW").take(3).toList
    val filledChannel2 = channel2
        .replace("(drink)", drink)
        .replace("(c1)", consonants(0).toString)
        .replace("(c2)", consonants(1).toString)
        .replace("(c3)", consonants(2).toString)
    tvSelect(filledChannel2, watchChannel4)
  }

  def watchChannel4(): Unit = {
    val filledChannel4 = channel4
      .replace("(country)", country)
      .replace("(numDead)", numDead)
      .replace("(neighborhood)", neighborhood)
    tvSelect(filledChannel4, watchChannel5)
  }

  def watchChannel5(): Unit = {
    val products = Random.shuffle(List(
      "shampoo",
      "toilet paper",
      "canned spaghetti",
      "frozen waffles",
      "soup",
      "disposable diapers",
      "hand lotion",
      "ice cream",
      "potato chips",
      "wine",
      "vitamins"
    )).take(3)
    val filledChannel5 = channel5
      .replace("(product1)", products(0))
      .replace("(product2)", products(1))
      .replace("(product3)", products(2))
    tvSelect(filledChannel5, watchChannel7)
  }

  def watchChannel7(): Unit = {
    val filledChannel7 = channel7
      .replace("(country)", country)
      .replace("(numDead)", numDead)
      .replace("(neighborhood)", neighborhood)
    tvSelect(filledChannel7, watchChannel9)
  }

  def watchChannel9(): Unit = {
    if (gameContext.testFlag(WATCHED_FAMILY_FEUD)) {
      tvSelect(channel9_truncated, watchChannel11)
    } else {
      gameContext.setFlag(WATCHED_FAMILY_FEUD)
      tvSelect(channel9, watchChannel11)
    }
  }

  def watchChannel11(): Unit = {
    tvSelect(channel11, watchChannel13)
  }

  def watchChannel13(): Unit = {
    val letters: List[Char] =
      Random.shuffle("ABCDEFGHIJKLMNOPQRSTUVWXYZ").take(2).toList
    val number = Random.shuffle("0123456789").head
    val filledChannel13 = channel13
      .replace("(letter1)", letters(0).toString)
      .replace("(letter2)", letters(1).toString)
      .replace("(number)", number.toString)
    tvSelect(filledChannel13, watchChannel2Repeat)
  }

  def watchChannel2Repeat(): Unit = {
    printAndWait(channel2_repeat)
    printAndWait("To Node Nightmare 1")
  }
}
