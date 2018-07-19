package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec

class StackOverflowSpec extends PropSpec with Utils {
  property("Array size under collection size condition") {
    val constraintArray = getArrayString(List.range(1, 300))
    val declareConstraintArray =
      s"""
                {
                  let a : Array[Int] = $constraintArray
                  a[0]
                }
        """.stripMargin
    testCompiledExpressionWithOptionalEvaluation(declareConstraintArray, compilationSuccess = true,
      evaluationSuccess = Option(true), expectedValue = Option(1))
  }

  property("Array size bigger than collection size condition") {
    val bigArray = getArrayString(List.range(1, 100000))
    val declareBigArray =
      s"""
                {
                  let a : Array[Int] = $bigArray
                }
       """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(declareBigArray, compilationSuccess = false)
  }

  property("Array size way bigger than collection size condition") {
    val hugeArray = getArrayString(List.range(1, 1000000))
    val declareHugeArray =
      s"""
                {
                  let a : Array[Int] = $hugeArray
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(declareHugeArray, compilationSuccess = false)
  }

  property("String causing overflow") {
    val string = generateRandomString(1000000)
    val stringDeclaration =
      s"""
                {
                  let a = "$string"
                }
        """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(stringDeclaration, compilationSuccess = false)
  }

  property("Lambda increasing collection depth") {
    val constraintArray = getArrayString(List.range(1, 301))
    val mapCollection =
      s"""
                {
                  def toArray(x : Int, array : Array[Int]) : Array[Int] = {
                    array
                  }
                  def toArray2(x : Int, array : Array[Any]) : Array[Any] = {
                    array
                  }

                  let J : Array[Int] = $constraintArray
                  let A : Array[Int] = $constraintArray
                  let B = A.map(lamb(x : Int) = toArray(x, J))
                  let C = A.map(lamb(x : Int) = toArray2(x, B))
                  let D = A.map(lamb(x : Int) = toArray2(x, C))
                  let E = A.map(lamb(x : Int) = toArray2(x, D))
                  let F = A.map(lamb(x : Int) = toArray2(x, E))
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(mapCollection, compilationSuccess = false)
  }

  property("Dumb zip attack") {
    val constraintArray = getArrayString(List.range(1, 301))
    val mapCollection =
      s"""
                {
                  def toArray(x : Int, array : Array[Int]) : Array[Int] = {
                    array
                  }
                  def toArray2(x : Int, array : Array[Array[Int]]) : Array[Array[Int]] = {
                    array
                  }

                  def toArray3(x : Int, array : Array[Array[Array[Int]]]) : Array[Array[Array[Int]]] = {
                    array
                  }

                  let J : Array[Int] = $constraintArray
                  let A : Array[Int] = $constraintArray
                  let B = A.map(lamb(x : Int) = toArray(x, J))
                  let C = A.map(lamb(x : Int) = toArray2(x, B))
                  cet D = A.map(lamb(x : Int) = toArray3(x, C))
                }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(mapCollection, compilationSuccess = false)
  }
}
