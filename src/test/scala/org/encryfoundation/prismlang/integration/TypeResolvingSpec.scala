package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class TypeResolvingSpec extends PropSpec with Matchers with Utils {
  property("Array declaration") {
    val arrayDeclaration =
      """
                {
                  let a : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                }
      """.stripMargin

    compiled(arrayDeclaration).isSuccess shouldBe true
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

    compiled(sumBoolAndInt).isSuccess shouldBe false
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

    compiled(boolAndInt).isSuccess shouldBe false
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

    compiled(boolAndString).isSuccess shouldBe false
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

    compiled(intOrString).isSuccess shouldBe false
  }
  //FIXME Int on byte division not working
  property("Division Int on byte") {
    val byte_num = 101.toByte

    val divideIntOnByte =
      s"""
                {
                  let v : Byte = $byte_num
                  let a : Int = 1010
                  let w = a/v
                  w
                }
      """.stripMargin

    val tryInByteDivision = compiled(divideIntOnByte)
    tryInByteDivision.isSuccess shouldBe true
    val evaluatedExpression = eval(tryInByteDivision.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 10
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

    val tryIntDivision = compiled(intDivisionRight)
    tryIntDivision.isSuccess shouldBe true
    val evaluatedExpression = eval(tryIntDivision.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 4
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

    val tryIntDivision = compiled(intDivisionMod)
    tryIntDivision.isSuccess shouldBe true
    val evaluatedExpression = eval(tryIntDivision.get)
    evaluatedExpression.isSuccess shouldBe true
    evaluatedExpression.get shouldEqual 6
  }
  //FIXME Zero division somehow compiles successfully
  property("Zero Division") {
    val zeroDivision =
      """
                {
                  let b = 21
                  let c = 0
                  let d = b/c
                  d
                }
      """.stripMargin

    compiled(zeroDivision).isSuccess shouldBe false
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

    compiled(intDivisionWrong).isSuccess shouldBe false
  }

  property("Array with variable of another type") {
    val mixedTypesInArray =
      """
                {
                  let G : Array[Int] = Array(1,2,5,true)
                }
      """.stripMargin

    compiled(mixedTypesInArray).isSuccess shouldBe false
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

    compiled(arrayAddInt).isSuccess shouldBe false
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

    compiled(arrayConcatenation).isSuccess shouldBe false
  }

  property("Resolving in conditional statement") {
    val ifElseStatementTypeResolving =
      """
                {
                  let a : Int = 7
                  let c : Byte = (120).toByte
                  if (c > a){
                    let b = c
                  } else {
                    let b = a
                  }
                }
      """.stripMargin

    compiled(ifElseStatementTypeResolving).isSuccess shouldBe true
  }

  property("Sum bytes exceed byte boundaries") {
    val sumOfBytes =
      """
                {
                  let a : Byte = (120).toByte
                  let b : Byte = (101).toByte
                  let c : Byte = a + b
                  c
                }
      """.stripMargin

    compiled(sumOfBytes).isSuccess shouldBe false
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

    compiled(longMultiplication).isSuccess shouldBe false
  }
  //FIXME Long multiplication exceeds boundary
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

    compiled(longMultiplication).isSuccess shouldBe false
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

    compiled(sumStringAndInt).isSuccess shouldBe false
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

    compiled(sumStringAndInt).isSuccess shouldBe false
  }
  //FIXME falls with Exception PInt != PString
  property("Sum of two strings should be string") {
    val string = generateRandomString(15)
    val string2 = generateRandomString(15)
    val sumStringAndString =
      s"""
                {
                  let a = "$string"
                  let b = "$string2"
                  let c = b + a
                }
        """.stripMargin

    compiled(sumStringAndString).isSuccess shouldBe true
  }
  
  property("Array of Any") {
    val arrayOfAnyUpCast =
      """
                {
                  let a : Array[Any] = Array(1,2,3,5)
                }
      """.stripMargin

    compiled(arrayOfAnyUpCast).isSuccess shouldBe true
  }
}
