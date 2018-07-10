package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.parser.Parser
import org.scalatest.{Matchers, PropSpec}

class SyntaxSpec extends PropSpec with Matchers with Parser with TestCompiler {

  property(testName = "SSA"){
    val correctOrder = """
                {let a = 10
               | let b = 10
               | let c = a + b}
      """.stripMargin

    val doubleAssignment = """
                {let a = 10
                | let a = 15}
      """.stripMargin

    val variableBeforeAssignment ="""
               {let c = a + 10
                | let a = 25}
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
        |let a : Int = 5
        |let b : Int = 7
        |let g = sum(a,b)}""".stripMargin

    val successMusk = Seq(true, false, false, false, true)
//    val tries = Seq(correctOrder, doubleAssignment, variableBeforeAssignment, functionBeforeAssignment, functionCorrectOrder)
//      .map(e =>compileExpr(parse(e).get.head))
//      .zip(successMusk).
//      map(e => e._1.isSuccess shouldBe e._2)
    compileExpr(parse(functionCorrectOrder).get.head).isSuccess shouldBe true
  }

  property (testName = "Infinite Loop"){

  }

  property (testName = "stack overflow"){
    val hugeArrayRepresent = getArrayString(List.range(1,320))//.map(e => e.toLong))
    val light =
      s"""{let a : Array[Int] = $hugeArrayRepresent}""".stripMargin
    val successMusk = Seq(false)
    val tries = Seq(light)
      .map(e => compileExpr(parse(e).get.head))
      .zip(successMusk).
      map(e => e._1.isSuccess shouldBe e._2)
  }

  property(testName = "Type resolving"){

    val query0 = "{let a : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)}"

    val query1 = """
        {let b = 20
        |let c = true
        |let d = b + c}
      """.stripMargin

    val byte_num = 192.toByte

    val query2 = s"""
        {let v : Byte = $byte_num
         | let a : Int = 77
         | let w = a/v
        }
      """.stripMargin

// FIXME in later version add Error pop up for user
//    val query3 =
//      """{let G : Array[Int] = Array(1,2,5,7.6)}
//      """.stripMargin

    val query4 =
      """
        {let V : Array[Int] = Array(1,2,3)
        |let W : Array[Int] = Array(3,4,5)
        |let Z = V + W}
      """.stripMargin

    val successMusk = Seq(true, false, false, false, false)
    val tries = Seq(query0, query1, query2, query4)
      .map(e => compileExpr(parse(e).get.head))
      .zip(successMusk).
      map(e => e._1.isSuccess shouldBe e._2)
  }

  property(testName = "syntax constructions"){
    val ifElseStatement = """{}"""
    val worongIfElse = """{}"""
    val conditionalVariableDeclaration = """{}"""
  }

  def getArrayString(sample : List[Any]) : String = sample.mkString("Array(", ", ", ")")

}
