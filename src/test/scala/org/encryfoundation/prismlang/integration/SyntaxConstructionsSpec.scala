package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class SyntaxConstructionsSpec extends PropSpec with Matchers with Utils {

  property("Byte Cast") {
    val toByteCast =
      """
                {
                  let b : Byte = (101).toByte
                }
      """.stripMargin

    compiled(toByteCast).isSuccess shouldBe true
  }
  property("Int Upper Boundary check") {
    val longMin = Long.MinValue
    val letLongMinNumber =
      s"""
                {
                  let a : Int = $longMin
                }
        """.stripMargin

    compiled(letLongMinNumber).isSuccess shouldBe true
  }

  property("Int Lower Boundary check") {
    val longMax = Long.MaxValue

    val letLongMaxNumber =
      s"""
                {
                  let a : Int = $longMax
                }
        """.stripMargin

    compiled(letLongMaxNumber).isSuccess shouldBe true
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
                }
      """.stripMargin

    compiled(conditionalVariableDeclaration).isSuccess shouldBe true
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
                }
      """.stripMargin

    compiled(arrayDeclaration).isSuccess shouldBe true
  }

  property("String declaration correct length") {
    val string = generateRandomString(90)
    val stringDeclaration =
      s"""
                {
                  let a : String = "$string"
                }
        """.stripMargin

    compiled(stringDeclaration).isSuccess shouldBe true
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
}
