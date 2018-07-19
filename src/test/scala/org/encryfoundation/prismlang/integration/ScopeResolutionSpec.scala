package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec

class ScopeResolutionSpec extends PropSpec with Utils {

  property("Variable same to function argument") {
    val variableSameToArgument =
      """
                {
                  def sum(a: Int, b: Int): Int = {
                    a + b
                  }
                  let a = sum(5, 7)
                  a
                }
      """.stripMargin
    testCompiledExpressionWithOptionalEvaluation(variableSameToArgument, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(12))
  }

  property("Variable same to Variable inside function") {
    val variableSameToAnother =
      """
                {
                  def dumbTwice(a : Int): Int = {
                    let g = 2
                    g * a
                  }

                  let g = dumbTwice(5)
                  g
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(variableSameToAnother, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(10))
  }

  property("Reference variable outside of scope") {
    val referenceOutOfScope =
      """
                {
                  def dumbTwice(a : Int): Int = {
                    a * g
                  }

                  let g = 2
                  let res = dumbTwice(5)
                  res
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(referenceOutOfScope, compilationSuccess = false)
  }

  property("Reference variable inside function") {
    val referenceOutOfScope =
      """
                {
                  def dumbTwice(a : Int): Int = {
                  let g = 2
                    a * g
                  }

                  let f = dumbTwice(5)
                  let res = g + f
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(referenceOutOfScope, compilationSuccess = false)
  }
}
