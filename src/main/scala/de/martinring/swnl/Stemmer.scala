package de.martinring.swnl

import scala.io.Source
import scala.util.matching.Regex

object Stemmer {
  val endings =
    Map(Noun -> Map("s" -> "",
                    "ses" -> "s",
                    "xes" -> "x",
                    "zes" -> "z",
                    "ches" -> "ch",
                    "shes" -> "sh",
                    "men" -> "man",
                    "ies" -> "y"),
        Verb -> Map("s" -> "s",
                    "ies" -> "y",
                    "es" -> "e",
                    "es" -> "",
                    "ed" -> "e",
                    "ed" -> "",
                    "ing" -> "e",
                    "ing" -> ""),
        Adjective -> Map("er" -> "",
                         "est" -> "",
                         "er" -> "e",
                         "est" -> "e"),
        Adverb -> Map())
  
  private val Exception =
    """(.*) (.*)""".r
  
  private def readExceptions(pos: POS): Map[String, String] = 
    Source.fromURL(getClass.getResource("/dict/" + pos.id + ".exc"))
          .getLines
          .collect {
            case Exception(a,b) => a -> b
          }.toMap 
          
  private lazy val exceptions =
    Map(Noun -> readExceptions(Noun),
        Verb -> readExceptions(Verb),
        Adjective -> readExceptions(Adjective),
        Adverb -> readExceptions(Adverb))
  
  def possibleStems(pos: POS)(s: String): Iterator[String] = 
    endings(pos).collect {
      case (suffix,ending) if s endsWith suffix =>
        s.dropRight(suffix.length) + ending
    }.iterator ++ new Iterator[String] {
      lazy val value = exceptions(pos).get(s)
      var used = false
      def hasNext = !used && value.isDefined
      def next = {
        used = true
        value.get
      }
    }    
  
  private object LookUp {
    def unapply(s: String): Option[Option[Index.Item]] = Some(Index.find(s))
  }
  
  def stems(pos: POS)(s: String): Iterator[Index.Item] =
    possibleStems(pos)(s).collect{
      case LookUp(w) if w.isDefined => w.get
    }
  
  def stems(s: String): Iterator[Index.Item] =
    stems(Noun)(s) ++ stems(Verb)(s) ++ stems(Adjective)(s) ++ stems(Adverb)(s)   
}
