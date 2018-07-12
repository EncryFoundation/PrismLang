package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class SyntaxConstructionsSpec extends PropSpec with Matchers with Utils {

  property("Byte Cast") {
    val toByteCast =
      """
                {
                  let b : Byte = (101).toByte
                  b
                }
      """.stripMargin

    val tryToByteCast = compiled(toByteCast)
    tryToByteCast.isSuccess shouldBe true
    val evaluatedExpression = eval(tryToByteCast.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 101.toByte
  }
  property("Int Upper Boundary check") {
    val longMin = Long.MinValue
    val letLongMinNumber =
      s"""
                {
                  let a : Int = $longMin
                  a
                }
        """.stripMargin

    val tryLongMin = compiled(letLongMinNumber)
    tryLongMin.isSuccess shouldBe true
    val evaluatedExpression = eval(tryLongMin.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual longMin
  }

  property("Int Lower Boundary check") {
    val longMax = Long.MaxValue

    val letLongMaxNumber =
      s"""
                {
                  let a : Int = $longMax
                  a
                }
        """.stripMargin

    val tryLongMax = compiled(letLongMaxNumber)
    tryLongMax.isSuccess shouldBe true
    val evaluatedExpression = eval(tryLongMax.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual longMax
  }

  property("Conditional variable declaration") {
    val conditionalVariableDeclaration =
      """
                {
                  let f : Bool = if (7==7){
                    true
                  } else {
                    false
                  }
                  f
                }
      """.stripMargin

    val tryConditionalDeclaration = compiled(conditionalVariableDeclaration)
    tryConditionalDeclaration.isSuccess shouldBe true
    val evaluatedExpression = eval(tryConditionalDeclaration.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual true
  }

  property("Wrong conditional variable declaration") {
    val wrongConditionalVariable =
      """
                {
                  let g : Int = 47 if (5<2) else 22
                }
      """.stripMargin

    compiled(wrongConditionalVariable).isSuccess shouldBe false
  }
  //FIXME array out of bounds case below lower bound
  property("Array lower boundary violation") {
    val arrayDeclaration =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let b = A[-1]
                }
      """.stripMargin

    compiled(arrayDeclaration).isSuccess shouldBe false
  }
  //FIXME array out of bounds case above upper bound
  property("Array upper boundary violation") {
    val arrayDeclaration =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let b = A[10]
                }
      """.stripMargin
    compiled(arrayDeclaration).isSuccess shouldBe false
  }

  property("Array access in boundaries") {
    val arrayDeclaration =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let b = A[5]
                  b
                }
      """.stripMargin

    val tryGetElement = compiled(arrayDeclaration)
    tryGetElement.isSuccess shouldBe true
    val evaluatedExpression = eval(tryGetElement.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 6
  }

  property("String declaration correct length") {
    val string = generateRandomString(90)
    val stringDeclaration =
      s"""
                {
                  let a : String = "$string"
                  a
                }
        """.stripMargin

    val tryString = compiled(stringDeclaration)
    tryString.isSuccess shouldBe true
    val evaluatedExpression = eval(tryString.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual string
  }

  property("String declaration incorrect length") {
    val string = generateRandomString(109)
    val stringDeclaration =
      s"""
                {
                  let a : String= "$string"
                }
        """.stripMargin

    compiled(stringDeclaration).isSuccess shouldBe false
  }

  property("String subscription") {
    val string = generateRandomString(90)
    val stringDeclaration =
      s"""
                {
                  let a : String = "$string"
                  let b : String = a[1]
                }
        """.stripMargin

    compiled(stringDeclaration).isSuccess shouldBe false
  }

  property("Correct recursion ") {
    val factorialRecursive =
      s"""
                {
                  def factorial(n: Int): Int = if (n==0){
                      1
                    } else {
                      n * factorial(n-1)
                    }

                  let a = factorial(5)
                  a
                }
        """.stripMargin

    val tryFactorial = compiled(factorialRecursive)
    tryFactorial.isSuccess shouldBe true
    val evaluatedExpression = eval(tryFactorial.get)
    evaluatedExpression.isSuccess shouldBe false
  }

  property("Lambda test") {
    val mapCollection =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let B = A.map(lamb(x : Int) = x + 1)
                  B.size
                }
      """.stripMargin

    val tryMapCollection = compiled(mapCollection)
    tryMapCollection.isSuccess shouldBe true
    val evaluatedExpression = eval(tryMapCollection.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 9
  }
}
