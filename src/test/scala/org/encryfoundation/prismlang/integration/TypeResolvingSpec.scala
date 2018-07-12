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

  property("Division Int on byte") {
    val byte_num = 101.toByte

    val divideIntOnByte =
      s"""
                {
                  let v : Byte = $byte_num
                  let a : Int = 77
                  let w = a/v
                }
      """.stripMargin

    compiled(divideIntOnByte).isSuccess shouldBe false
  }

  property("Int division without mod") {
    val intDivisionRight =
      """
                {
                  let b = 20
                  let c = 5
                  let d = b/c
                }
      """.stripMargin
    compiled(intDivisionRight).isSuccess shouldBe true
  }

  property("Int division with mod") {
    val intDivisionWrong =
      """
                {
                  let b = 20
                  let c = 3
                  let d = b/c
                }
      """.stripMargin

    compiled(intDivisionWrong).isSuccess shouldBe true
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

  //FIXME falls with Exception PCollection(PAny) != PCollection(PInt)
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
