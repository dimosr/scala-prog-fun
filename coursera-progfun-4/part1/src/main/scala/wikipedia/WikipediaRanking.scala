package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String) {
  /**
    * @return Whether the text of this article mentions `lang` or not
    * @param lang Language to look for (e.g. "Scala")
    */
  def mentionsLanguage(lang: String): Boolean = text.split(' ').contains(lang)
}

object WikipediaRanking {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("WikipediaAnalysis")
  val sc: SparkContext = new SparkContext(conf)

  val wikiRdd: RDD[WikipediaArticle] = sc.textFile(WikipediaData.filePath)
                                            .map(WikipediaData.parse)

  /**
    * Returns the number of articles on which the language `lang` occurs.
   */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int = {
    def languageOcc(language: String, article: WikipediaArticle) = {
      if(article.mentionsLanguage(language)) 1 else 0
    }

    rdd.aggregate(0)((currentSum, article) => currentSum + languageOcc(lang, article), _ + _)
  }

  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] = {
    langs.map(language => (language, occurrencesOfLang(language, rdd)))
      .sortBy(_._2)
      .reverse
  }

  /* Compute an inverted index of the set of articles, mapping each language
   * to the Wikipedia pages in which it occurs.
   */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] = {
    rdd.flatMap(article => {
        val languages = article.text.split(" ")
                        .filter(language => langs.contains(language))
                        .distinct
        languages.map((article, _))
      })
      .groupBy(_._2)
      .mapValues(articlesWithWords => articlesWithWords.map(_._1))
  }

  /* (2) Compute the language ranking again, but now using the inverted index.
   */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] = {
    index.map(languageArticles => (languageArticles._1, languageArticles._2.size))
      .collect().toList
      .sortBy(_._2).reverse
  }

  /* (3) Use `reduceByKey` so that the computation of the index and the ranking are combined.
   */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] = {
    rdd.flatMap(article => {
        val languages = article.text.split(" ")
          .filter(language => langs.contains(language))
          .distinct
        languages.map((_, 1))
      })
      .reduceByKey(_ + _)
      .collect().toList
      .sortBy(_._2).reverse
  }

  def main(args: Array[String]) {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
