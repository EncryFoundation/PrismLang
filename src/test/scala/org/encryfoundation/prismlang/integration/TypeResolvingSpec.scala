package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec


class TypeResolvingSpec extends PropSpec with Utils {
  property("Array declaration") {
    val arrayDeclaration =
      """
                {
                  let a : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  a[6]
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayDeclaration, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(7))
  }

  property("Sum Boolean and Int") {
    val sumBoolAndInt =
      """
                {
                  let b = 20
                  let c = true
                  let d = b + c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sumBoolAndInt, compilationSuccess = false)
  }

  property("Logical And Boolean and Int") {
    val boolAndInt =
      """
                {
                  let b = 200
                  let c = true
                  let d = b && c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(boolAndInt, compilationSuccess = false)
  }

  property("Logical And Boolean and String") {
    val boolAndString =
      """
                {
                  let b = "true"
                  let c = true
                  let d = c && b
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(boolAndString, compilationSuccess = false)
  }

  property("Logical Or Int and String") {
    val intOrString =
      """
                {
                  let b = "true"
                  let c = 100
                  let d = c && b
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(intOrString, compilationSuccess = false)
  }

  property("Division Int on byte") {
    val divideIntOnByte =
      s"""
                {
                  let v : Byte = (101).toByte
                  let a : Int = 1010
                  let w = a/v
                  w
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(divideIntOnByte, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(10))
  }

  property("Int division without mod") {
    val intDivisionRight =
      """
                {
                  let b = 20
                  let c = 5
                  let d = b/c
                  d
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(intDivisionRight, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(4))
  }

  property("Int division with mod") {
    val intDivisionMod =
      """
                {
                  let b = 20
                  let c = 3
                  let d = b/c
                  d
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(intDivisionMod, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(6))
  }

  property("Zero Division Const") {
    val zeroDivision =
      """
                {
                  let b = 21 / 0
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(zeroDivision, compilationSuccess = false)
  }

  property("Zero Division Reference") {
    val zeroDivision =
      """
                {
                  let b = 21
                  let c = 0
                  let d = b/c
                  d
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(zeroDivision, compilationSuccess = true,
      evaluationSuccess = Option(false))
  }

  property("Division int on string(number)") {
    val intDivisionWrong =
      """
                {
                  let b = 20
                  let c = "3"
                  let d = b/c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(intDivisionWrong, compilationSuccess = false)
  }

  property("Array with variable of another type") {
    val mixedTypesInArray =
      """
                {
                  let G : Array[Int] = Array(1,2,5,true)
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(mixedTypesInArray, compilationSuccess = false)
  }

  property("Sum Array and Int") {
    val arrayAddInt =
      """
                {
                  let V : Array[Int] = Array(1,2,3)
                  let w : Int = 7
                  let Z = V + w
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayAddInt, compilationSuccess = false)
  }

  property("Sum of two arrays") {
    val arrayConcatenation =
      """
                {
                  let V : Array[Int] = Array(1,2,3)
                  let W : Array[Int] = Array(3,4,5)
                  let Z = V + W
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayConcatenation, compilationSuccess = false)
  }

  property("Resolving in conditional statement") {
    val ifElseStatementTypeResolving =
      """
                {
                  let a : Int = 7
                  let c : Byte = (120).toByte
                  if (c > a){
                    c
                  } else {
                    a
                  }
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(ifElseStatementTypeResolving, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(120.toByte))
  }

  property("resolve type for add operation") {
    val sources =
      """
                {
                  let a : Byte = (1).toByte
                  let b : Byte = (2).toByte
                  let c : Byte = a + b
                  c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(3))
  }

  property("resolve type for let") {
    val sources =
      """
                {
                  let a = 120
                  let b = 120
                  let c : Byte = a + b
                  c
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(false))
  }

  property("Sum bytes exceed byte boundaries") {
    val sumOfBytes =
      """
                {
                  let a : Byte = (120).toByte + (101).toByte
                  a
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sumOfBytes, compilationSuccess = false)
  }

  property("Int multiplication exceeds upper boundary") {
    val longMax = Long.MaxValue
    val longMultiplication =
      s"""
                {
                  let a = $longMax * $longMax
                  a
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(longMultiplication, compilationSuccess = false)
  }

  property("Int multiplication exceeds lower boundary") {
    val longMax = Long.MaxValue
    val longMin = Long.MinValue

    val longMultiplication =
      s"""
                {
                  let a = $longMax * $longMin
                  a
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(longMultiplication, compilationSuccess = false)
  }

  property("Sum String and Int") {
    val string = generateRandomString(10)
    val sumStringAndInt =
      s"""
                {
                  let a = "$string"
                  let b = 25
                  let c = a + b
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sumStringAndInt, compilationSuccess = false)
  }

  property("Sum array and Int") {
    val string = generateRandomString(10)
    val sumStringAndInt =
      s"""
                {
                  let a = "$string"
                  let b = Array(1,2,3)
                  let c = b + a
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sumStringAndInt, compilationSuccess = false)
  }

  property("Sum of two strings should be string") {
    val string = generateRandomString(15)
    val string2 = generateRandomString(15)
    val sumStringAndString =
      s"""
                {
                  let a = "$string"
                  let b = "$string2"
                  let c = a + b
                  c
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sumStringAndString, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(string + string2))
  }

  property("Array of Any not allowed") {
    val arrayOfAnyUpCast =
      """
                {
                  let a : Array[Any] = Array(1,2,3,5)
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(arrayOfAnyUpCast, compilationSuccess = false)
  }

  property("Nested collection limit") {
    val source =
      """
        |let coll = Array(Array(1), Array(Array(1), Array(1, 2)))
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(source, compilationSuccess = false)
  }

}
