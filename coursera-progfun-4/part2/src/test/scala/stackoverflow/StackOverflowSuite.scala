package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    import StackOverflow._

    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120

    val postings = Array(
      new Posting(1, 300, Option(302), Option.empty, 1, Option("Java")),
      new Posting(2, 301, Option.empty, Option(300), 2, Option.empty),
      new Posting(2, 302, Option.empty, Option(300), 15, Option.empty),

      new Posting(1, 400, Option.empty, Option.empty, 1, Option("Ruby")),
      new Posting(2, 401, Option.empty, Option(400), 2, Option.empty),
      new Posting(2, 402, Option.empty, Option(400), 4, Option.empty),
      new Posting(2, 403, Option.empty, Option(400), 0, Option.empty),

      new Posting(1, 500, Option.empty, Option.empty, 1, Option("Haskell"))
    )
    val postingsRDD = sc.parallelize(postings)

    val groupedPostings = Array(
      (300, Array(
        (new Posting(1, 300, Option(302), Option.empty, 1, Option("Java")), new Posting(2, 301, Option.empty, Option(300), 2, Option.empty)),
        (new Posting(1, 300, Option(302), Option.empty, 1, Option("Java")), new Posting(2, 302, Option.empty, Option(300), 15, Option.empty) )
      ).toIterable),
      (400, Array(
        (new Posting(1, 400, Option.empty, Option.empty, 1, Option("Ruby")), new Posting(2, 401, Option.empty, Option(400), 2, Option.empty)),
        (new Posting(1, 400, Option.empty, Option.empty, 1, Option("Ruby")), new Posting(2, 402, Option.empty, Option(400), 4, Option.empty)),
        (new Posting(1, 400, Option.empty, Option.empty, 1, Option("Ruby")), new Posting(2, 403, Option.empty, Option(400), 0, Option.empty))
      ).toIterable)
    )
    val groupedPostingsRDD = sc.parallelize(groupedPostings)

    val scoredPostings = Array(
      (new Posting(1, 300, Option(302), Option.empty, 1, Option("Java")), 15),
      (new Posting(1, 400, Option.empty, Option.empty, 1, Option("Ruby")), 4)
    )
    val scoredPostingsRDD = sc.parallelize(scoredPostings)
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupedPosting groups by question ID successfully") {
    val groupedPostings = testObject.groupedPostings(testObject.postingsRDD)
                                    .collect().toMap

    assert(groupedPostings.keys.size == 2, "There are 2 questions")
    assert(groupedPostings.get(300).get.size == 2, "There are 2 answers for question 300")
    assert(groupedPostings.get(400).get.size == 3, "There are 3 answers for question 400")
  }

  test("scoredPostings calculates scores of questions correctly") {
    val scores = testObject.scoredPostings(testObject.groupedPostingsRDD)
                             .map(entry => (entry._1.id, entry._2) ).collect().toMap

    assert(scores.keys.size == 2, "There are 2 questions")
    assert(scores.get(300).get == 15, "Question 300 has highest score 15")
    assert(scores.get(400).get == 4, "Question 400 has highest score 4")
  }

  test("vectorPostings calculates kmeans vectors correctly") {
    val vectors = testObject.vectorPostings(testObject.scoredPostingsRDD)
                              .collect().toMap

    assert(vectors.keys.size == 2, "There are 2 questions")
    assert(vectors.get(1 * 50000).get == 15, "Question with Java had 15 highest answer score")
    assert(vectors.get(6 * 50000).get == 4, "Question with Ruby had 4 highest answer score")

  }


}
