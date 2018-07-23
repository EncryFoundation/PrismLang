package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec

class SyntaxConstructionsSpec extends PropSpec with Utils {
  property("Logical or") {
    val logicalOr =
      """
                {
                  let a = true || false
                  a
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(logicalOr, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Logical and") {
    val logicalAnd =
      """
                {
                  let a = true && false
                  a
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(logicalAnd, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(false))
  }

  property("Logical negation") {
    val negation =
      """
                {
                  let a = not true
                  let b = !a
                  b
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(negation, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Logical expression") {
    val negation =
      """
                {
                  let a = !(false && true)
                  let b = !false || !true
                  a == b
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(negation, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Byte Cast") {
    val toByteCast =
      """
                {
                  let b : Byte = (101).toByte
                  b
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(toByteCast, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(101.toByte))
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

    testCompiledExpressionWithOptionalEvaluation(letLongMinNumber, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(longMin))
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

    testCompiledExpressionWithOptionalEvaluation(letLongMaxNumber, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(longMax))
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

    testCompiledExpressionWithOptionalEvaluation(conditionalVariableDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Wrong conditional variable declaration") {
    val wrongConditionalVariable =
      """
                {
                  let g : Int = 47 if (5<2) else 22
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(wrongConditionalVariable, compilationSuccess = false)
  }

  property("Conditional variable in one line") {
    val shortConditionalVariable =
      """
                {
                  let b = 7
                  let c = 5
                  let a = if (c>b) c else b
                  a
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(shortConditionalVariable, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(7))
  }


  property("Array lower boundary violation") {
    val arrayDeclaration =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let b = A[-1]
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(false))
  }

  property("Array upper boundary violation") {
    val arrayDeclaration =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let b = A[10]
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(false))
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

    testCompiledExpressionWithOptionalEvaluation(arrayDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(6))
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

    testCompiledExpressionWithOptionalEvaluation(stringDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(true), Option(string))
  }

  property("String declaration incorrect length") {
    val string = generateRandomString(109)
    val stringDeclaration =
      s"""
                {
                  let a : String= "$string"
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(stringDeclaration, compilationSuccess = false)
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

    testCompiledExpressionWithOptionalEvaluation(stringDeclaration, compilationSuccess = false)
  }

  property("Correct recursion also not allowed in prism") {
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

    testCompiledExpressionWithOptionalEvaluation(factorialRecursive, compilationSuccess = false)
  }

  property("Lambda map") {
    val mapCollection =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let B = A.map(lamb(x : Int) = x + 1)
                  B.size
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(mapCollection, compilationSuccess = true,
      evaluationSuccess = Option(true), Option(9))
  }

  property("Lambda exists") {
    val existsInCollection =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  let f = A.exists(lamb(x : Int) = x > 8)
                  f
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(existsInCollection, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("In collection") {
    val inCollection =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  9 in A
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(inCollection, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Not in collection") {
    val notInCollection =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  11 in A
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(notInCollection, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(false))
  }

  property("All of Collection") {
    val allOf =
      """
                {
                  let A : Array[Int] = Array(1,2,3,4)
                  allOf(Array(2 > 1, 1 == 1, 3 != 4, 10 < 100, 3 in A,true))
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(allOf, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(true))
  }

  property("Array depth 2") {
    val collectionDepth2 =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5)
                  let B : Array[Int] = Array(6, 7, 8, 9)
                  let C : Array[Array[Int]] = Array(A,B)
                  C[0][2]
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(collectionDepth2, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(3))
  }

  property("Array depth 3") {
    val collectionDepth2 =
      """
                {
                  let A : Array[Int] = Array(1, 2, 3, 4, 5)
                  let B : Array[Int] = Array(6, 7, 8, 9)
                  let C : Array[Array[Int]] = Array(A,B)
                  let D : Array[Array[Array[Int]]] = Array(C,C,C)
                  D[2][2][1]
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(collectionDepth2, compilationSuccess = false)
  }
}
