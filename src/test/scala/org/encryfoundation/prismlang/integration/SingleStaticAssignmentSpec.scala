package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class SingleStaticAssignmentSpec extends PropSpec with Matchers with Utils {
  property(testName = "Correct variable order") {
    val correctOrder =
      """
                {
                  let a = 10
                  let b = 10
                  let c = a + b
                }
      """.stripMargin

    compiled(correctOrder).isSuccess shouldBe true
  }

  property(testName = "Variable reassignment") {

    val doubleAssignment =
      """
                {
                  let a = 10
                  let a = 15
                }
      """.stripMargin

    compiled(doubleAssignment).isSuccess shouldBe false
  }
  property(testName = "Reference variable before assignment") {
    val variableBeforeAssignment =
      """
                {
                  let c = a + 10
                  let a = 25
                }
      """.stripMargin

    compiled(variableBeforeAssignment).isSuccess shouldBe false
  }
  property(testName = "Reference function before assignment") {
    val functionBeforeAssignment =
      """
                {
                  let c = sum(4,5)
                  def sum(a: Int, b: Int): Int = {
                    a + b
                  }
                }
               """.stripMargin

    compiled(functionBeforeAssignment).isSuccess shouldBe false
  }
  property(testName = "Reference function in correct order") {
    val functionCorrectOrder =
      """
                {
                  def sum(a: Int, b: Int): Int = {
                    a + b
                  }

                  let c : Int = 5
                  let d : Int = 7
                  let g = sum(c,d)
                }
      """.stripMargin

    compiled(functionCorrectOrder).isSuccess shouldBe true
  }

  property(testName = "Conditional Statement with reassignment") {

    val ifElseStatementReassignment =
      """
                {
                  let a : Int = 7
                  let b : Int = 8
                  let c : Byte = (120).toByte
                  if (c > a){
                    let b = c
                  } else {
                    let b = a
                  }
                }
      """.stripMargin

    compiled(ifElseStatementReassignment).isSuccess shouldBe false
  }
}
