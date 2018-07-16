package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class ScopeResolutionSpec extends PropSpec with Matchers with Utils {
  //Fixme different scopes gives: Wrong number of arguments, 2 required, 1 given.
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

    val tryVariableSameToArgument = compiled(variableSameToArgument)
    tryVariableSameToArgument.isSuccess shouldBe true
    val evaluatedExpression = eval(tryVariableSameToArgument.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 12
  }
  //FIXME on evaluation : java.lang.Exception: 2 and List(5) does not support `Mul` operation
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

    val tryVariableSameToAnother = compiled(variableSameToAnother)
    tryVariableSameToAnother.isSuccess shouldBe true
    val evaluatedExpression = eval(tryVariableSameToAnother.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 10
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

    compiled(referenceOutOfScope).isSuccess shouldBe false
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

    compiled(referenceOutOfScope).isSuccess shouldBe false
  }
}
