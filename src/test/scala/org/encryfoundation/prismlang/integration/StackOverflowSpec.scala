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
    //FIXME Out of memory (heap space) with 100M array size
    val hugeArray = getArrayString(List.range(1, 1000000))
    val declareHugeArray =
      s"""
                {
                  let a : Array[Int] = $hugeArray
                }
        """.stripMargin

    compiled(declareHugeArray).isSuccess shouldBe false
  }
}
