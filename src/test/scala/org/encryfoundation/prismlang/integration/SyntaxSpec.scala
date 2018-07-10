package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.parser.Parser
import org.scalatest.{Matchers, PropSpec}

class SyntaxSpec extends PropSpec with Matchers with Parser with TestCompiler {

  property(testName = "SSA") {
    val correctOrder =
      """
                {let a = 10
                |let b = 10
                |let c = a + b}
      """.stripMargin

    val doubleAssignment =
      """
                {let a = 10
                |let a = 15}
      """.stripMargin

    val variableBeforeAssignment =
      """
               {let c = a + 10
                |let a = 25}
      """.stripMargin

    val functionBeforeAssignment =
      """{let c = sum(4,5)
        |def sum(a: Int, b: Int): Int = {
        |        a + b
        |    }
        |}""".stripMargin

    val functionCorrectOrder =
      """{def sum(a: Int, b: Int): Int = {
        |        a + b
        |    }
        |let c : Int = 5
        |let d : Int = 7
        |let g = sum(c,d)}""".stripMargin
    //FIXME variable reassignment works inside if-else statement
    val ifElseStatementReassignment =
      """{let a : Int = 7
        |let b : Int = 8
        |let c : Byte = (120).toByte
        |if (c > a){
        |  let b = c
        |} else {
        |   let b = a
        |   }
        |}""".stripMargin

    compiled(correctOrder).isSuccess shouldBe true
    compiled(doubleAssignment).isSuccess shouldBe false
    compiled(variableBeforeAssignment).isSuccess shouldBe false
    compiled(functionBeforeAssignment).isSuccess shouldBe false
    compiled(functionCorrectOrder).isSuccess shouldBe true
    compiled(ifElseStatementReassignment).isSuccess shouldBe false
  }

  property(testName = "Infinite Loop") {

  }

  property(testName = "stack overflow") {
    val constraintArray = getArrayString(List.range(1, 300))
    val bigArray = getArrayString(List.range(1, 100000))

    val declareConstraintArray =
      s"""{let a : Array[Int] = $constraintArray}""".stripMargin

    val declareBigArray =
      s"""{let a : Array[Int] = $bigArray}""".stripMargin

    compiled(declareConstraintArray).isSuccess shouldBe true
    compiled(declareBigArray).isSuccess shouldBe false

    val hugeArray = getArrayString(List.range(1, 1000000))

    val declareHugeArray =
      s"""{let a : Array[Int] = $hugeArray}""".stripMargin

    compiled(declareHugeArray).isSuccess shouldBe false
  }

  property(testName = "Type resolving") {

    val arrayDeclaration = "{let a : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)}"

    val sumBoolAndInt =
      """
        {let b = 20
        |let c = true
        |let d = b + c}
      """.stripMargin

    val byte_num = 101.toByte

    val divideIntOnByte =
      s"""
        {let v : Byte = $byte_num
         |let a : Int = 77
         |let w = a/v
        }
      """.stripMargin

    val intDivisionRight =
      """
        {let b = 20
        |let c = 5
        |let d = b/c}
      """.stripMargin

    val intDivisionWrong =
      """
        {let b = 20
        |let c = 3
        |let d = b/c}
      """.stripMargin

    // FIXME in later version add Error pop up for user (for usage of float variables)
    val mixedTypesInArray =
      """{let G : Array[Int] = Array(1,2,5,true)}
      """.stripMargin

    val arrayAddInt =
      """
        {let V : Array[Int] = Array(1,2,3)
        |let w : Int = 7
        |let Z = V + w}
      """.stripMargin

    val arrayConcatenation =
      """
        {let V : Array[Int] = Array(1,2,3)
        |let W : Array[Int] = Array(3,4,5)
        |let Z = V + W}
      """.stripMargin

    compiled(arrayDeclaration).isSuccess shouldBe true
    compiled(sumBoolAndInt).isSuccess shouldBe false
    compiled(divideIntOnByte).isSuccess shouldBe false
    compiled(intDivisionRight).isSuccess shouldBe true
    compiled(intDivisionWrong).isSuccess shouldBe false
    compiled(mixedTypesInArray).isSuccess shouldBe false
    compiled(arrayAddInt).isSuccess shouldBe false
    compiled(arrayConcatenation).isSuccess shouldBe false
  }

  property(testName = "syntax constructions") {

    val toByteCast = "let b : Byte = (101).toByte"
    val longMin = Long.MinValue
    val longMax = Long.MaxValue
    //FIXME Long.MinValue assignment throws exception
    val letLongMinNumber =
      s"""{let a : Int = $longMin}""".stripMargin

    val letLongMaxNumber =
      s"""{let a : Int = $longMax}""".stripMargin

    val conditionalVariableDeclaration =
      """{let f : Bool = if (7==7)
        |{
        | true
        | } else {
        |false
        | }
        |}""".stripMargin
    //FIXME Python ternary operator works like let g : Int = 47
    val wrongConditionalVariable = "{let g : Int = 47 if (5<2) else 22}"

    val ifElseStatementTypeResolving =
      """{let a : Int = 7
        |let c : Byte = (120).toByte
        |if (c > a){
        |  let b = c
        |} else {
        |   let b = a
        |   }
        |}""".stripMargin

    compiled(toByteCast).isSuccess shouldBe true
    compiled(letLongMinNumber).isSuccess shouldBe true
    compiled(letLongMaxNumber).isSuccess shouldBe true
    compiled(conditionalVariableDeclaration).isSuccess shouldBe true
    compiled(wrongConditionalVariable).isSuccess shouldBe false
    compiled(ifElseStatementTypeResolving).isSuccess shouldBe true
  }

  def getArrayString(sample: List[Any]): String = sample.mkString("Array(", ", ", ")")

  def compiled(prismScript: String) = compileExpr(parse(prismScript).get.head)

}
