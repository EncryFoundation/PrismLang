package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class StackOverflowSpec extends PropSpec with Matchers with Utils {
  property("Array size under collection size condition") {
    val constraintArray = getArrayString(List.range(1, 300))
    val declareConstraintArray =
      s"""
                {
                  let a : Array[Int] = $constraintArray
                }
        """.stripMargin
    compiled(declareConstraintArray).isSuccess shouldBe true
  }

  property("Array size bigger than collection size condition") {
    val bigArray = getArrayString(List.range(1, 100000))
    val declareBigArray =
      s"""
                {
                  let a : Array[Int] = $bigArray
                }
       """.stripMargin

    compiled(declareBigArray).isSuccess shouldBe false
  }

  property("Array size way bigger than collection size condition") {
    val hugeArray = getArrayString(List.range(1, 1000000))
    val declareHugeArray =
      s"""
                {
                  let a : Array[Int] = $hugeArray
                }
        """.stripMargin

    compiled(declareHugeArray).isSuccess shouldBe false
  }

  property("String causing overflow") {
    val string = generateRandomString(1000000)
    val stringDeclaration =
      s"""
                {
                  let a = "$string"
                }
        """.stripMargin

    compiled(stringDeclaration).isSuccess shouldBe false
  }

  property("Lambda increasing collection depth") {
    val constraintArray = getArrayString(List.range(1, 301))
    val mapCollection =
      s"""
                {
                  def toArray(x : Int, array : Array[Int]) : Array[Int] = {
                    array
                  }

                  def toArray2(x : Int, array : Array[Any]) : Array[Any] = {
                    array
                  }

                  let A : Array[Int] = $constraintArray
                  let B = A.map(lamb(x : Int) = toArray(x, A))
                  let C = A.map(lamb(x : Int) = toArray2(x, B))
                  let D = A.map(lamb(x : Int) = toArray2(x, C))
                  let E = A.map(lamb(x : Int) = toArray2(x, D))
                }
      """.stripMargin

    val tryMapCollection = compiled(mapCollection)
    tryMapCollection.isSuccess shouldBe true
  }
}
