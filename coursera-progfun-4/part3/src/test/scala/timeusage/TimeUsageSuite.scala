package timeusage

import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("dfSchema() should create a list with the first column string and the rest double") {
    val schema = TimeUsage.dfSchema(List("firstCol", "secondCol", "thirdCol"))

    val firstCol = schema.head
    val secondCol = schema.tail.head
    val thirdCol = schema.tail.tail.head
    assert(firstCol.dataType == StringType)
    assert(firstCol.name == "firstCol")
    assert(secondCol.dataType == DoubleType)
    assert(secondCol.name == "secondCol")
    assert(thirdCol.dataType == DoubleType)
    assert(thirdCol.name == "thirdCol")
  }

  test("classifiedColumns() should return the correct classification of columns based on prefix") {
    val classification = TimeUsage.classifiedColumns(List("t01_sample", "t1803000_sample", "t07xx_sample"))

    assert(classification._1 === List(new Column("t01_sample"), new Column("t1803000_sample")))
    assert(classification._2.isEmpty)
    assert(classification._3 === List(new Column("t07xx_sample")))
  }

}
