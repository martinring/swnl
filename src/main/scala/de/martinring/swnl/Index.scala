package de.martinring.swnl

import scala.io.Source

object Index {  
  case class Item(line: String) {
    private val tokens = 
      line.split(' ').iterator
    
    val lemma = tokens.next       
    
    val pos = tokens.next match {
      case "n" => Noun
      case "v" => Verb
      case "a" => Adjective
      case "r" => Adverb
    }
    
    val synsetCount = tokens.next.toInt
    
    val pointerCount = tokens.next.toInt 
    
    override def toString =
      lemma
  }        
    
  private def read(pos: POS): Array[Item] = 
    Source.fromURL(getClass.getResource("/dict/index." + pos.id))
          .getLines
          .filter(!_.startsWith("  "))
          .map(Item(_))
          .toArray          
  
  private lazy val indices = 
    Map(Noun -> read(Noun),
        Verb -> read(Verb),
        Adjective -> read(Adjective),
        Adverb -> read(Adverb))
  
  def find(pos: POS)(v: String): Option[Item] = {
    val a = indices(pos)
    var p = a.length / 2
    var o = a.length / 4
    while(true) {
      a(p).lemma.compareTo(v) match {
        case x if x == 0 => return Some(a(p))
        case x if x < 0 => if (o != 0) {
          p += o
          o /= 2
        } else return None
        case _ => if (o != 0) {
          p -= o
          o /= 2
        } else return None
      }            
    }
    return None
  }  
  
  def find(v: String): Option[Item] = 
    find(Noun)(v) orElse 
    find(Verb)(v) orElse 
    find(Adjective)(v) orElse 
    find(Adverb)(v)    
}