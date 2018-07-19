package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class SingleStaticAssignmentSpec extends PropSpec with Matchers with Utils {
  property("Correct variable order") {
    val correctOrder =
      """
                {
                  let a = 10
                  let b = 10
                  let c = a + b
                  c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(correctOrder, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(20))
  }

  property("Variable reassignment") {

    val doubleAssignment =
      """
                {
                  let a = 10
                  let a = 15
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(doubleAssignment, compilationSuccess = false)
  }

  property("Reference variable before assignment") {
    val variableBeforeAssignment =
      """
                {
                  let c = a + 10
                  let a = 25
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(variableBeforeAssignment, compilationSuccess = false)
  }

  property("Reference function before assignment") {
    val functionBeforeAssignment =
      """
                {
                  let c = sum(4,5)
                  def sum(a: Int, b: Int): Int = {
                    a + b
                  }
                }
               """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(functionBeforeAssignment, compilationSuccess = false)
  }
  property("Reference function in correct order") {
    val functionCorrectOrder =
      """
                {
                  def sum(a: Int, b: Int): Int = {
                    a + b
                  }

                  let c : Int = 5
                  let d : Int = 7
                  let g = sum(c,d)
                  g
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(functionCorrectOrder, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(12))
  }

  property("Conditional Statement with reassignment") {

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

    testCompiledExpressionWithOptionalEvaluation(ifElseStatementReassignment, compilationSuccess = false)
  }
}
