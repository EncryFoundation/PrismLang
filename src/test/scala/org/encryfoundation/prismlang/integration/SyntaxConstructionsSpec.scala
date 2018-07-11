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
}
