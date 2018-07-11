package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class TypeResolvingSpec extends PropSpec with Matchers with Utils {
  property(testName = "Array declaration") {
    val arrayDeclaration =
      """
                {
                  let a : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
                }
      """.stripMargin

    compiled(arrayDeclaration).isSuccess shouldBe true
  }

  property(testName = "Sum Boolean and Int") {
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

  property(testName = "Division Int on byte") {
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

  property(testName = "Int division without mod") {
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

  property(testName = "Int division with mod") {
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

  property(testName = "Array with variable of another type") {
    val mixedTypesInArray =
      """
                {
                  let G : Array[Int] = Array(1,2,5,true)
                }
      """.stripMargin

    compiled(mixedTypesInArray).isSuccess shouldBe false
  }
  property(testName = "Sum Array and Int") {
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
  property(testName = "Sum of two arrays") {
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

  property(testName = "Resolving in conditional statement") {
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
}
