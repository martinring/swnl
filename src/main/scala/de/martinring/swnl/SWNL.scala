package de.martinring.swnl 

import scala.io.Source

sealed abstract class POS(val id: String)
object Noun extends POS("noun")
object Verb extends POS("verb")
object Adjective extends POS("adj")
object Adverb extends POS("adv")

object Main extends App {
  def timed(b: => Unit) {
    val t0 = System.nanoTime
    b
    println((System.nanoTime - t0) / 1000000 + "ms")  
  }
  
  timed {
    println(Index.find("mouse"))
  }
  timed {
    println(Index.find("house"))
  }
  timed {
    val it = Stemmer.stems("accompanied")
    while(it.hasNext) println(it.next)
  }
  timed {
    val it = Stemmer.stems("banned")
    while(it.hasNext) println(it.next)
  }
  timed {
    val it = Stemmer.stems("went")
    println(it.next)
  }
  
  
  
  
}