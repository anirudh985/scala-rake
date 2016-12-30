import com.sun.net.httpserver.Authenticator.Failure

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Created by aj on 12/30/16.
  */
object RAKE {
  private val sentenceDelimiter = """[\\[\\]\n.!?,;:\t\\-\\"\\(\\)\\\'\u2019\u2013]""".r
  private val maxWordsInPhrase: Int = 5
  private val minCharLength: Int = 1
//  private val stopWords = Source.fromFile("./SmartStoplist.txt").getLines().drop(1).toSet
  private val stopWords = Source.fromInputStream(getClass.getResourceAsStream("/SmartStoplist.txt")).getLines().drop(1).toSet

//  def readFile(filename: String): Try[Iterator[String]] = {
//    Try(Source.fromFile(filename).getLines)
//  }
//
//  private def loadStopWords(): Set[String] = {
//    readFile("./SmartStoplist.txt") match {
//      case Success(lines) => lines.drop(1).toSet
//      case Failure(f) => throw f
//    }
//  }

  private def splitTextToSentences(text: String): List[String] = {
    sentenceDelimiter.split(text).toList
  }

  private def getPhrasesForSentence(listOfWords: List[String])(predicate: String => Boolean): List[List[String]] = {
    listOfWords match {
      case Nil => Nil
      case x :: xs =>
        val phrase = listOfWords takeWhile predicate
        if(phrase.isEmpty || phrase.length > maxWordsInPhrase){
          getPhrasesForSentence(listOfWords.drop(1))(predicate)
        }
        else{
          phrase :: getPhrasesForSentence(listOfWords.drop(phrase.length + 1))(predicate)
        }
    }
  }

  private def isAcceptableString(word: String): Boolean = {
    if(word.length < minCharLength) return false
    if(!isAplha(word)) return false
    if(stopWords.contains(word)) return false
    return true
  }

  private def isAplha(word: String): Boolean = {
    word.matches("[a-z]+")
  }


  private def generateCandidateKeywords(sentences: List[String]): List[List[String]] = {
    val splittedSentences: List[List[String]] = sentences.map(sentence => sentence.trim.split("\\s+").map(_.toLowerCase).toList)
    splittedSentences.flatMap(sentenceList => getPhrasesForSentence(sentenceList)(isAcceptableString))
  }

  private def calculateWordScores(listOfPhrases: List[List[String]]): Map[String, Double] = {
    val completeListOfWords = listOfPhrases.flatten
    val wordFreqList: List[(String, Int)] = completeListOfWords.map(word => (word, 1))
    val wordDegreeListPartial: List[(String, Int)] = listOfPhrases.flatMap{ phrase =>
      val degree = phrase.length - 1
      phrase.map(word => (word, degree))
    }
    val wordFreqMap: Map[String, Int] = wordFreqList.groupBy(k => k._1).mapValues(l => l.map(_._2).sum)
    val wordDegreeMap: Map[String, Int] = (wordFreqList ++ wordDegreeListPartial).groupBy(k => k._1).mapValues(l => l.map(_._2).sum)
    wordFreqMap.keys.map(word => word -> wordDegreeMap(word).toDouble / wordFreqMap(word)).toMap
  }

  private def calculateScoresForPhrase(listOfPhrases: List[List[String]], wordScores: Map[String, Double]) = {
    listOfPhrases.map(phrase => phrase.mkString(" ") -> phrase.map(wordScores(_)).sum).toMap
  }

  def run(text: String): List[(String, Double)] = {
    val listOfSentences = splitTextToSentences(text)
    val listOfPhrases = generateCandidateKeywords(listOfSentences)
    println(listOfPhrases)
    val wordScores = calculateWordScores(listOfPhrases)
    println(wordScores)
    val phraseScores = calculateScoresForPhrase(listOfPhrases, wordScores)
    println(phraseScores)
    val orderedPhrases = phraseScores.toList.sortBy(x => x._2).reverse
    println(orderedPhrases)
    orderedPhrases
  }

}
